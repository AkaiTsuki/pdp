;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname repeats-in) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;report-repeats : LON -> LON
;takes a list of numbers and returns a list consisting of only 
;those numbers that appear more than once consecutively

;(report-repeats (list 12 12 3 4 7 7 7 12 5 7 7 8 8 12 12 12 15 12))
;= (list 12 7 7 8 12)

;report-repeats-acc : LON n -> LON
;Given a sublist slst of lon
;Where n is the last number that occurs in above slst in lon.
;Returns a list of number that appear more than once consecutively in lon



(define (report-repeats l)
  (local (
          (define (report-repeats-acc slst n s)
            (cond
              [(empty? slst) empty]
              [else (if (and (= n (first slst)) (not (= n s)))
                        (cons (first slst) (report-repeats-acc (rest slst) (first slst) n))
                        (report-repeats-acc (rest slst) (first slst) n)
                        )]))
          )
    (report-repeats-acc l 0 0)))