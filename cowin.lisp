;;; You need to set the *TOKEN* to access the private APIs
;;; Either get it from the browser by observing the bearer token sent on the Co-WIN website,
;;; or use the VALIDATE-MOBILE function below

(defpackage #:cowin
  (:use #:cl))

(in-package #:cowin)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "drakma")
  (ql:quickload "cl-json")
  (ql:quickload "cl-ppcre")
  (ql:quickload "cl-smtp")
  (ql:quickload "ironclad"))

(defun lookup (name object &key (test #'equalp))
  (cdr (assoc name object :test test)))

(defun lookup-fn (name &key (test #'equalp))
  (lambda (object)
    (lookup name object :test test)))

(defparameter *base-url* "https://cdn-api.co-vin.in/api")

(defvar *token* nil)
(defvar *captcha* nil)

(defun cowin-url (path)
  (concatenate 'string *base-url* path))

(define-condition api-request-error (error)
  ((status-code :initarg :status-code)
   (body :initarg :body)))

(defun api-request (path &key (method :get) parameters json (token *token*))
  (let (content
        content-type)
    (cond ((and (eql method :post) parameters)
           (setf content (drakma::alist-to-url-encoded-string parameters :utf-8 #'drakma:url-encode)
                 content-type "application/x-www-form-urlencoded"
                 parameters nil))
          (json
           (setf content (json:encode-json-to-string json)
                 content-type "application/json"))
          (t nil))
    (let ((drakma:*text-content-types* '(("application" . "json") ("text"))))
      (multiple-value-bind (response-body status-code)
          (drakma:http-request (cowin-url path)
                               :method method
                               :parameters parameters
                               :content content
                               :content-type content-type
                               :additional-headers `(,@(when token
                                                         `(("Authorization" . ,(format nil "Bearer ~A" token)))))
                               :user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:88.0) Gecko/20100101 Firefox/88.0")
        (if (and (>= status-code 200) (< status-code 300))
            (json:decode-json-from-string response-body)
            (progn
              (format t "Got non-2xx response: ~A~%" status-code)
              (princ response-body)
              (terpri)
              (error 'api-request-error :status-code status-code :body response-body)))))))

(defun sha256 (string)
  (let ((digester (ironclad:make-digest 'ironclad:sha256)))
    (ironclad:update-digest digester (map '(vector (unsigned-byte 8)) #'char-code string))
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest digester))))

(defun generate-otp (mobile &optional (secret "U2FsdGVkX1/0iTxx/ZnkuYxbwhlyNeChnCLVXqZWPN8l9kThHnc+BtLJk4S1ytGHesZjfEUlGpONICdlndRXmw=="))
  (lookup :txn-id
          (api-request "/v2/auth/generateMobileOTP"
                       :method :post
                       :json `(("mobile" . ,mobile)
                               ("secret" . ,secret)))))

(defun confirm-otp (otp txn-id)
  (let ((otp-sha (sha256 otp)))
    (lookup :token
            (api-request "/v2/auth/validateMobileOtp"
                         :method :post
                         :json `(("otp" . ,otp-sha)
                                 (txn-id . ,txn-id))))))

(defun validate-mobile (mobile)
  (let ((txn-id (generate-otp mobile)))
    (format t "txn-id is: ~A~%" txn-id)
    (princ "Please provide OTP: ")
    (force-output)
    (setf *token* (confirm-otp (read-line) txn-id))))

(defun get-districts-for-state (state-id)
  (lookup :districts (api-request (format nil "/v2/admin/location/districts/~A" state-id))))

(defun get-all-districts ()
  (mapcar (lambda (district)
            (cons (lookup :district--name district) (lookup :district--id district)))
          (loop
            for state-id from 1 upto 36
            nconc (get-districts-for-state state-id))))

(defvar *all-districts* nil)

(defun update-all-districts ()
  (setf *all-districts* (get-all-districts)))

(defun get-district-id (name)
  (when (null *all-districts*)
    (update-all-districts))
  (lookup name *all-districts*))

(defun get-centers (date &key pincode district-name)
  (assert (or pincode district-name)
          (pincode district-name)
          "One of pincode or district-name must be given")
  (assert (not (and pincode district-name))
          nil
          "Both pincode and district-name can't be given")
  (let* ((district-id (when district-name
                        (get-district-id district-name))))
    (flet ((api-path (trailer)
             (with-output-to-string (out)
               (princ "/v2/appointment/sessions/" out)
               (when (null *token*)
                 (princ "public/" out))
               (princ trailer out))))
      (lookup :centers
              (api-request (cond
                             (district-id (api-path "calendarByDistrict"))
                             (pincode (api-path "calendarByPin")))
                           :parameters `(("date" . ,date)
                                         ,@(when pincode
                                             `(("pincode" . ,pincode)))
                                         ,@(when district-id
                                             `(("district_id" . ,(princ-to-string district-id))))))))))

(defun get-beneficiaries ()
  (lookup :beneficiaries (api-request "/v2/appointment/beneficiaries")))

(defun get-captcha ()
  (let ((string (lookup :captcha
                        ;;(api-request "/v2/auth/getRecaptcha" :method :post)
                        *sample-captcha-response*)))
    (uiop/stream:with-temporary-file (:stream out
                                      :pathname pathname
                                      :keep t
                                      :direction :output
                                      :type "svg")
      (write-string string out)
      pathname)))

(defun save-captcha ()
  (let ((captcha-pathname (get-captcha)))
    (format t "Captcha path is: ~A~%" captcha-pathname)
    (format t "Enter captcha value: ")
    (force-output)
    (setf *captcha* (read-line))))

(defun book-appointment (center-id session-id beneficiaries slot &key (dose 1) (captcha *captcha*))
  (when (atom beneficiaries)
    (setf beneficiaries (list beneficiaries)))
  (let* ((payload `((center_id . ,center-id)
                    (session_id . ,session-id)
                    (beneficiaries . ,(coerce beneficiaries 'vector))
                    (slot . ,slot)
                    (dose . ,dose)
                    (captcha . ,captcha))))
    (api-request "/v2/appointment/schedule"
                 :method :post
                 :json payload)))


;;; Booking Flow

(defun session-has-availability (session)
  (and (plusp (lookup :available--capacity session))
       (= (lookup :min--age--limit session) 18)))

(defun available-sessions (sessions)
  (remove-if-not #'session-has-availability sessions))

(defun center-has-availability (center)
  (some #'session-has-availability (lookup :sessions center)))

(defun available-centers (centers)
  (remove-if-not #'center-has-availability centers))

(defun should-book-center (center)
  (declare (ignore center))
  t)

(define-condition appointment-booked (error)
  ())

(define-condition all-beneficiaries-booked (error)
  ())

(defun beneficiary-id (beneficiary)
  (lookup :beneficiary--reference--id beneficiary))

(defun book-for-date (date beneficiaries &key pincode district-name)
  (format t "Checking for ~A on ~A~%" (or pincode district-name) date)
  (let ((available-centers (available-centers (get-centers date
                                                           :pincode pincode
                                                           :district-name district-name))))
    (format t "Available centers:")
    (print available-centers)
    (terpri)
    (terpri)
    (dolist (center available-centers nil)
      (when (should-book-center center)
        (let ((center-name (lookup :name center :test #'string-equal)))
          (let* ((center-id (lookup :center--id center))
                 (available-sessions (available-sessions (lookup :sessions center))))
            (dolist (session available-sessions)
              (let ((session-date (lookup :date session)))
                (book-appointment center-id
                                  (lookup :session--id session)
                                  (mapcar #'beneficiary-id beneficiaries)
                                  (first (lookup :slots session)))
                (format t "Appointment booked for ~A on ~A~%"
                        center-name
                        session-date)
                (error 'appointment-booked)))))))))

(defun beneficiary-has-appointment (beneficiary)
  (lookup :appointments beneficiary))

(defun book (dates &key pincode district-name (sleep-between-calls 5))
  (let ((beneficiaries (remove-if #'beneficiary-has-appointment (get-beneficiaries))))
    (format t "Beneficiaries to book for: ~S~%" (mapcar (lookup-fn :name) beneficiaries))
    (if beneficiaries
        (dolist (date dates)
          (book-for-date date beneficiaries
                         :pincode pincode
                         :district-name district-name)
          (sleep sleep-between-calls))
        (error 'all-beneficiaries-booked))))

(defun book-loop (dates &key
                          pincode
                          district-name
                          (sleep-between-calls 5)
                          (sleep-between-iterations 120))
  (handler-case
      (loop
        (book dates
              :pincode pincode
              :district-name district-name
              :sleep-between-calls sleep-between-calls)
        (sleep sleep-between-iterations))
    ((or appointment-booked all-beneficiaries-booked) (c)
      (class-name (class-of c)))))
