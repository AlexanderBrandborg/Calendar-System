; NAME: Alexander Brandborg
; AAU MAIL: abran13@student.aau.dk

;-------------Course Functions--------------------------
;;; Error function
;;; create binding for error
(define error #f)

;;; capture toplevel continuation
;;;  assign a function to error, allowing a variable number of arguments to
;;;  be passed
(call-with-current-continuation (lambda (k)
             (set! error
               (lambda error-arguments
                 (display ">>>> ERROR ")
                 (newline)
                 (k error-arguments)))
             'done))

;;; Higher order functions
(define (filter pred lst)
  (reverse (filter-help pred lst '())))

(define (filter-help pred lst res)
  (cond ((null? lst) res)
        ((pred (car lst)) 
           (filter-help pred (cdr lst)  (cons (car lst) res)))
        (else 
           (filter-help pred (cdr lst)  res))))

(define (negate p)
  (lambda (x) 
    (if (p x) #f #t)))

(define (find-in-list pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) (car lst))
        (else (find-in-list pred (cdr lst)))))

(define (flip f)
  (lambda (x y)
    (f y x)))

(define (curry2 f)
  (lambda(x)
    (lambda(y)
      (f x y))))

(define (accumulate-right f init lst)
  (if (null? lst)
      init
      (f (car lst) (accumulate-right f init (cdr lst)))))

(define (mymap f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (mymap f (cdr lst)))))

;;;;Sorting
;;; (merge! a b less?)
;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept vectors.

(define (sort:merge! a b less?)
    (define (loop r a b)
	(if (less? (car b) (car a))
	    (begin
		(set-cdr! r b)
		(if (null? (cdr b))
		    (set-cdr! b a)
		    (loop b a (cdr b)) ))
	    ;; (car a) <= (car b)
	    (begin
		(set-cdr! r a)
		(if (null? (cdr a))
		    (set-cdr! a b)
		    (loop a (cdr a) b)) )) )
    (cond
	((null? a) b)
	((null? b) a)
	((less? (car b) (car a))
	    (if (null? (cdr b))
		(set-cdr! b a)
		(loop b a (cdr b)))
	    b)
	(else ; (car a) <= (car b)
	    (if (null? (cdr a))
		(set-cdr! a b)
		(loop a (cdr a) b))
	    a)))



;;; (sort! sequence less?)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(define (sort:sort! seq less?)
    (define (step n)
	(cond
	    ((> n 2)
		(let* ((j (quotient n 2))
		       (a (step j))
		       (k (- n j))
		       (b (step k)))
		    (sort:merge! a b less?)))
	    ((= n 2)
		(let ((x (car seq))
		      (y (cadr seq))
		      (p seq))
		    (set! seq (cddr seq))
		    (if (less? y x) (begin
			(set-car! p y)
			(set-car! (cdr p) x)))
		    (set-cdr! (cdr p) '())
		    p))
	    ((= n 1)
		(let ((p seq))
		    (set! seq (cdr seq))
		    (set-cdr! p '())
		    p))
	    (else
		'()) ))
    (if (vector? seq)
	(let ((n (vector-length seq))
	      (vec seq))
	  (set! seq (vector->list seq))
	  (do ((p (step n) (cdr p))
	       (i 0 (+ i 1)))
	      ((null? p) vec)
	    (vector-set! vec i (car p)) ))
	;; otherwise, assume it is a list
	(step (length seq)) ))

;;; (sort sequence less?)
;;; sorts a vector or list non-destructively.  It does this by sorting a
;;; copy of the sequence.  My understanding is that the Standard says
;;; that the result of append is always "newly allocated" except for
;;; sharing structure with "the last argument", so (append x '()) ought
;;; to be a standard way of copying a list x.

(define (sort:sort seq less?)
    (if (vector? seq)
	(list->vector (sort:sort! (vector->list seq) less?))
	(sort:sort! (append seq '()) less?)))

; Alias
(define sort-list sort:sort)

;------------Constructors and accessors---------------
; Builds an associations list from a list of names and values
(define (ass-list-builder names values)
   (if(null? names)
       '()
   (cons (cons (car names) (car values))
         (ass-list-builder(cdr names) (cdr values))))  
)

(define (is-leap-year? year)
  (if (= ( modulo year 4) 0)
      (if (= ( modulo year 100) 0)
          (= ( modulo year 400) 0)
          #t)
      #f ))

; Defines a list where numerical values are paired
; with the number of days corresponding to the given month.
; Handles leap year.
(define (get-month-day-list year)
 (let ((feb-num (if (is-leap-year? year) 29 28)))
 (ass-list-builder (list 1 2 3 4 5 6 7 8 9 10 11 12)
                   (list 31 feb-num 31 30 31 30 31 31 30 31 30 31))))  


; Defines a list of numerical values paired with names of months
(define (month-name-list)
 (ass-list-builder (list 1 2 3 4 5 6 7 8 9 10 11 12)
                   (list "January" "Febuary" "March"
                         "April" "May" "June"
                         "July" "August" "September"
                         "October" "November" "December"))) 

; Accessesors member of association list by name
(define (find-in-ass-list list name)
  (cond ((null? list) #f)
        ((eq? (car(car list)) name) (cdr(car list)))
        (else (find-in-ass-list (cdr list) name))))

;Time accessor
(define (access-time time name)
  (cond ((eq? name "minute") (modulo time 100))
        ((eq? name "hour") (floor (/ (modulo time 10000) 100)))
        ((eq? name "day") (floor (/ (modulo time 1000000) 10000)))
        ((eq? name "month") (floor (/ (modulo time 100000000) 1000000)))
        ((eq? name "year") (floor (/ time  100000000)))
  (else (error ("No such element in time")))))

;Appointment accessor
(define (access-appointment app name)
  (let ((res (find-in-ass-list app name)))
    (if res res (error "No such element in appointment"))))

;Calendar accessor
(define (access-calendar cal name)
  (let ((res (find-in-ass-list cal name)))
    (if res res (error "No such element in calendar"))))

;Month list acessor
(define (access-month-list lst num)
  (let ((res (find-in-ass-list lst num)))
    (if res res (error "No such element in month-day-list"))))

; Gets name of month from its numerical value
(define (month-number->name num)
  (access-month-list  (month-name-list) num))

; Gets number of days in month
(define (days-in-month year month)
  (access-month-list (get-month-day-list year) month))

; Constructs a well formed time value
(define (time-constructor year month day hour . minute)
  (let ((minute (if (null? minute) 0 (car minute))))
  (cond ((or (not (integer? minute)) (< minute 0 ) (<= 60 minute))
         (error "Wrong minutes"))
        ((or (not (integer? hour)) (< hour 0) (<= 24 hour))
         (error "Wrong hour"))
        ((or (not (integer? day)) (< day 1) (< (days-in-month year month) day))
         (error "Wrong day"))
        ((or (not (integer? month)) (< month 1) (< 12 month))
         (error "Wrong month"))
        ((not (integer? year))
         (error "Wrong year"))
        (else
         (+ (* year 100000000)
            (+ (* month 1000000)
               (+ (* day 10000)
                  (+ (* hour 100) minute))))))))


; Constructs a well formed appointment association list
(define (appointment-constructor start end . text)
  (let ((text (if (null? text) "Not described" (car text))))
  (cond ((not (string? text))
         (error "Text not a string"))
        ((or (not (integer? start)) (not (integer? end)))
         (error "Time not a list"))
        (else
         (ass-list-builder (list "text" "start" "end")
                                (list text start end))))))

; Constructs a well formed calendar association list
(define (calendar-constructor appointments . sub-calendars)
  (cond ((or (null? list) (not (list? appointments)))
         (error "Appointments not a list"))
        ((or (null? list) (not (list? sub-calendars)))
         (error "Sub-calendars not a list"))
        (else
         (ass-list-builder
          (list "appointments" "sub-calendars")
          (list appointments sub-calendars)))))

;------------Calendar functionality-------------------

; Add appointment to existing calendar
(define (add-appointment cal app)
  (let ((apps (access-calendar cal "appointments"))
        (subs (access-calendar cal "sub-calendars")))
  (calendar-constructor (cons app apps) subs)))  

; Add a sub-calender to existing calendar
(define (add-sub-calendar cal sub)
  (let ((apps (access-calendar cal "appointments"))
        (subs (access-calendar cal "sub-calendars")))
  (calendar-constructor apps (cons sub subs))))

; Delete appointment from existing calendar based on predicate
(define (delete-appointment cal pred)
  (let ((apps (access-calendar cal "appointments"))
        (subs (access-calendar cal "sub-calendars")))
  (calendar-constructor (filter (negate pred) apps) subs))) 

; Delete sub-calendar from existing calendar based on predicate
(define (delete-sub-calendar cal pred)
  (let ((apps (access-calendar cal "appointments"))
        (subs (access-calendar cal "sub-calendars")))
  (calendar-constructor apps (filter (negate pred) subs)))) 

; Check if two appointments overlap in time
(define (appointments-overlap? app1 app2)
  (let ((start1 (access-appointment app1 "start"))
        (start2 (access-appointment app2 "start"))
        (end1 (access-appointment app1 "end"))
        (end2 (access-appointment app2 "end")))
  (and (<= start1 end2) (<= start2 end1)))) 

; Check if an appointment comes before another
(define (app-time-less? app1 app2)
  (let ((start1 (access-appointment app1 "start"))
        (start2 (access-appointment app2 "start")))
  (< start1 start2)))

; Find all appointments matching some predicate
(define (find-appointments cal pred)
  (let ((apps (access-calendar cal "appointments")))
  (filter pred apps))) 

;Finds first appointment upholding predicate in sorted list
(define (find-first-appointment cal pred)
  (let ((apps (access-calendar cal "appointments")))
  (find-in-list pred (sort-list apps app-time-less?)))) 

;Finds last appointment upholding predicate in sorted list
(define (find-last-appointment cal pred)
  (let ((apps (access-calendar cal "appointments")))
  (find-in-list  pred (sort-list apps (flip app-time-less?))))) 

;Flattens calendar and creates a new calendar element
(define (flatten-calendar cal)
  
  ;Flattens a calendar pulling all appointments up to top level
(define (flatten-list cal)
  (let ((apps (access-calendar cal "appointments"))
        (subs (access-calendar cal "sub-calendars")))
    (append apps(accumulate-right append '() (mymap flatten-list subs)))))
  
    (calendar-constructor (flatten-list cal)
                          (access-calendar cal "sub-calendars")))

;Checks for temporal overlap between two calendars
(define (calendars-overlap? cal1 cal2)

  ;Checks for overlap between two given lists of appointments
  (define (calendar-overlap-helper apps1 apps2)
    (cond ((or (null? apps1) (null? apps2))
           #f)
          ((appointments-overlap? (car apps1) (car apps2))
           #t)
          ((app-time-less? (car apps1) (car apps2))
           (calendar-overlap-helper (cdr apps1) apps2))
          (else
           (calendar-overlap-helper apps1 (cdr apps2)))))
  
  (let ((apps1 (access-calendar cal1 "appointments"))
         (apps2 (access-calendar cal2 "appointments")))
  (calendar-overlap-helper (sort-list apps1 app-time-less?)
                           (sort-list apps2 app-time-less?)))) 


; Maps a function on every appointment within a calendar and evaluates to a new calendar
 (define (map-appointments cal func)
  (letrec ((helper
            (lambda (x)
              (if (null? x) '() (append (func (car x)) (helper (cdr x)))))))
    (calendar-constructor
     (helper (access-calendar cal "appointments"))
     (access-calendar cal "sub-calendars"))))

;-----------HTML Functions-----------------------------------
; Produces an html string representing a calendar within a period of time
(define (present-calendar-html cal from-time to-time)

  ; Gets CCS style string for HTML representation
  (define CCS-style (string-append  "<style> #wrapper {white-space: normal;}"
                                    "#wrapper div {border: 3px solid black;"
                                    "display: inline-block; height: 200px;"
                                    "width: 200px; vertical-align:"
                                    "top; word-wrap: break-word;} </style>"))
  
  ; Gets a string from time representing time of day in the format hh:mm
  (define (time->string time)
    (let ((hour (number->string (access-time time "hour")))
          (minute (number->string (access-time time "minute"))))
      (string-append hour ":" minute)))
  
  ; Gets an html representation of an appointment
  (define (app->string app)
    (let* ((text (access-appointment app "text"))
           (start (access-appointment app "start"))
           (end (access-appointment app "end")))
      (string-append " <div> Text: "
                     text
                     " <br> Start: "
                     (time->string start)
                     " <br> End: "
                     (time->string end)
                     " </div>")))
  
  
  ; Get value of date, but without hours and minutes
  (define (get-day-value time) (floor (/ time 10000)))
  
  ; Get first date of next month in line
  (define (get-first-day-of-month date)
    (time-constructor
     (access-time date "year")
     (+ (access-time date "month") 1)
     1 0))
  
  ; Get the next date in line
  (define (get-next-day date)
    (let ((year (access-time date "year"))
          (month (access-time date "month"))
          (day (access-time date "day")))
      (cond ((and (= month 12) (= day 31))
             (time-constructor (+ year 1) 1 1 0))
            ((= day (days-in-month year month))
             (time-constructor year (+ month 1) 1 0))
            (else
             (time-constructor year month (+ day 1) 0)))))
  
  ; Get the last minute of a date
  (define (get-last-minute date)
    (let ((year (access-time date "year"))
          (month (access-time date "month"))
          (day (access-time date "day")))
      (time-constructor year month  day 23 59)))
  
  ; If an appointment stretches itself over a period,
  ; this will create an appointment for every day that period.
  ; This way a long appointment will also fill up the calendar visually.
  (define (multiple-day-apps app)
    (letrec((text (access-appointment app "text"))
            (start (access-appointment app "start"))
            (end (access-appointment app "end")))
      (if (= (get-day-value start) (get-day-value end))
          (cons app '())
          (cons (appointment-constructor start (get-last-minute start) text)
                (multiple-day-apps
                 (appointment-constructor (get-next-day start) end  text)))))) 
  
  ; Constructs HTML for every appointment within a list
  (define (insert-apps apps)
    (letrec ((helper
              (lambda (x)
                (if (null? x) "" (string-append (app->string (car x)) (helper (cdr x)))))))
      (if (null? apps)
          "<p> Nothing today! </p>"
          (helper apps))))
  
  ; Constructs HTML for day within a month
  (define (day-html apps from-time to-time)
    (let* ((from-month (access-time from-time "month"))
           (from-day (access-time from-time "day"))  
           (to-day (access-time to-time "day"))
           (header (string-append "<h1>" (number->string from-day) "</h1>"))
           (new-apps (filter
                      (lambda (x)
                        (= (access-time (access-appointment x "start") "day") from-day))
                      apps)))
      (if (= from-day to-day)
          (string-append header
                         " <section id=""wrapper""> "
                         (insert-apps new-apps)
                         " </section> " )  
          (string-append  header
                          " <section id=""wrapper""> "
                          (insert-apps new-apps)
                          " </section> "
                          (day-html apps (get-next-day from-time) to-time)))))  
  
  ; Constructs HTML for every month within a year
  (define (month-html apps from-time to-time)
    (let* ((year (access-time from-time "year"))
           (from-month (access-time from-time "month"))
           (to-month (access-time to-time "month"))
           (alt-to-time
            (time-constructor year from-month (days-in-month year from-month) 0))
           (header
            (string-append "<h1>" (month-number->name from-month) "</h1>"))
           (new-apps
            (filter (lambda (x)
                      (= (access-time (access-appointment x "start") "month") from-month)) apps )))
      (if (= from-month to-month)
          (string-append header
                         " <section> "
                         (day-html new-apps from-time to-time)
                         " </section> ")
          (string-append header
                         " <section> "
                         (day-html new-apps from-time alt-to-time)
                         " </section> "
                         (month-html apps (get-first-day-of-month from-time) to-time)))))
  
  ; Constructs HTML for every year within a period
  (define (year-html apps from-time to-time)
    (let* ((from-year (access-time from-time "year"))
           (to-year (access-time to-time "year"))
           (alt-to-time (time-constructor from-year 12 31 0))
           (alt-from-time (time-constructor (+ from-year 1) 1 1 0))
           (header (string-append "<h1>" (number->string from-year) "</h1>"))
           (new-apps (filter
                      (lambda (x)
                        (= (access-time (access-appointment x "start") "year") from-year))
                      apps ))) 
      (if (= from-year to-year) 
          (string-append header
                         " <section> "
                         (month-html new-apps from-time to-time)
                         " </section> ")
          (string-append header
                         " <section> "
                         (month-html new-apps from-time alt-to-time)
                         " </section> "
                         (year-html apps alt-from-time to-time)))))
  
  
  ; Gets subset of appointments within a calendar,
  ; which are within a defined block of time
  (define (sort-list-subset cal from-time to-time)
    (let* ((period (appointment-constructor from-time to-time))
           (pred ((curry2 appointments-overlap?) period)))
      (sort-list (find-appointments cal pred)  app-time-less?)))

  (let ((apps (sort-list-subset
             (map-appointments cal multiple-day-apps)
             from-time
             to-time)))
  (string-append CCS-style (year-html apps from-time to-time))))

; Writes an html string to a file
(define (write-html filename string)
  (with-output-to-file filename (lambda () (display string))))
        
;-----------Tests---------------------------------------
(define t1 (time-constructor 2016 10 1 0))
(define t2 (time-constructor 2017 1 4 23 59))

(define ap11 (appointment-constructor  (time-constructor 2016 10 26 9) (time-constructor 2016 10 26 16 30 )  "Work on code"))
(define ap12 (appointment-constructor  (time-constructor 2016 10 27 9) (time-constructor 2016 10 27 16 30) "Work on report"))
(define ap13 (appointment-constructor  (time-constructor 2016 10 27 16 30) (time-constructor 2016 10 27 16 45) "Turn in report"))
(define ap14 (appointment-constructor  (time-constructor 2016 10 27 19 ) (time-constructor 2016 10 27 19 30) "Victory pizza"))
(define ap15 (appointment-constructor (time-constructor 2017 1 1 0) (time-constructor 2017 1 5 23 59) "Vacation on smalltalk island"))

(define ap21 (appointment-constructor  (time-constructor 2016 10 31 0) (time-constructor 2016 10 31 23 59) "Haloween"))
(define ap22 (appointment-constructor  (time-constructor 2016 12 24 0) (time-constructor 2016 12 26 23 59) "Christmas days"))

(define ap31 (appointment-constructor  (time-constructor 2016 11 4 18) (time-constructor 2016 11 6 15) "Koyo"))

(define convention-calendar (calendar-constructor (list ap31)))
(define holiday-calendar (calendar-constructor (list ap21 ap22)))
(define code-calendar (calendar-constructor (list ap12 ap14 ap13 ap11 ap15) holiday-calendar convention-calendar))


; (add-appointment convention-calendar ap11)
; (add-sub-calendar convention-calendar code-calendar)
; (delete-appointment holiday-calendar (lambda (x) (eq? "Haloween" (access-appointment x "text") )))
; (delete-sub-calendar code-calendar (lambda (x) (eq? "Haloween" (access-appointment (car(access-calendar x "appointments")) "text" ))))
; (appointments-overlap? ap11 ap12)
; (appointments-overlap? ap11 ap11)
; (find-appointments code-calendar (lambda (x) (= 2016 (access-time (access-appointment x "start") "year"))))
; (find-first-appointment code-calendar (lambda (x) (= 2016 (access-time (access-appointment x "start") "year"))))
; (find-last-appointment code-calendar (lambda (x) (= 2016 (access-time (access-appointment x "start") "year"))))
; (flatten-calendar code-calendar)
; (calendars-overlap? holiday-calendar (flatten-calendar code-calendar))
; (present-calendar-html (flatten-calendar code-calendar) t1 t2)