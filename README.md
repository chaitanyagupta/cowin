# Co-WIN for Common Lisp

Check for vaccine availability or book an appointment on
[Co-WIN](https://www.cowin.gov.in/home).

USE THIS AT YOUR OWN RISK. See the LICENSE.

## Getting Started

1. Get a CL compiler like [SBCL](http://www.sbcl.org/)
2. Get [Quicklisp](https://www.quicklisp.org/beta/)
3. Run `sbcl --load cowin.lisp`

This will land your in a Lisp REPL. From there you can do the following.

## Check for availability

Check for available vaccination centers using a date and either a district name:

```cl
(available-centers (get-centers "08-05-2021" :district-name "Gurgaon"))
```

Or a pin code:

```cl
(available-centers (get-centers "08-05-2021" :pincode "122001"))
```

The date should be in DD-MM-YYYY format. `GET-CENTERS` searches for availabiliy
for seven days starting from the given date.

## Book an appointment

To book an appointment, first you need to validate your mobile:

```cl
(validate-mobile "xxx")  ;; xxx here is your mobile number
```

This will ask you to provide the OTP sent to your mobile. Once the mobile is
validated, the easiest way to book an appointment would be to run the
`BOOK-LOOP` function:

```
(book-loop (list "08-05-2021") :district-name "Gurgaon")
```

This will try to book an appointment for every beneficiary registered on the
validated mobile that doesn't already have an appointment. It will stop when an
appointment is booked or when the session expires (approx. 15 minutes).

## Changing availability or booking logic

A center is considered available if it at least one dose is available on any of
the dates returned and the age requirement is 18+.

To change the availability logic to suit your needs, for example, to change
the age requirement to 45+, modify `SESSION-HAS-AVAILABILITY` as follows:

```cl
(defun session-has-availability (session)
  (and (plusp (lookup :available--capacity session))
       (= (lookup :min--age--limit session) 45)))
```

To put a restriction on centers that can be booked, modify
`SHOULD-BOOK-CENTER`. For example, to restrict the centers by name and
availability date, you can do something like this:

```cl
(defun should-book-center (center)
  (and (member (lookup :name center) '("Fortis Memorial Resarch Inst" "Max Hospital")
               :test #'string-equal)
       (let ((sessions (available-sessions (lookup :sessions center))))
         (some (lambda (session)
                 (member (lookup :date session) '("08-05-2021" "09-05-2021")
                         :test #'string-equal))
               sessions))))
```