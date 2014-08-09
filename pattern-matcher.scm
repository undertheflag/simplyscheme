(require (lib "trace.ss"))
(load "~/Documents/Scheme/PLT Scheme v372/berkeley/simply.scm")

(define (match pattern sent)
  (match-using-known-values pattern sent '()))

(define (match-using-known-values pattern sent known-values)
  (cond ((empty? pattern)
         (if (empty? sent) known-values 'failed))
        ((special? (first pattern))
         (let ((placeholder (first pattern)))
           (match-special (first placeholder)
                          (bf placeholder)
                          (bf pattern)
                          sent
                          known-values)))
        ((empty? sent) 'failed)
        ((equal? (first pattern) (first sent))
         (match-using-known-values (bf pattern) (bf sent) known-values))
        (else 'failed)))

(define (special? wd)
  (member? (first wd) '(* & ? ! +)))

(define (match-special howmany name pattern-rest sent known-values)
  (let ((old-value (lookup name known-values)))
    (cond ((not (equal? old-value 'no-value))
           (if (length-ok? old-value howmany)
               (already-known-match
                old-value pattern-rest sent known-values)
               'failed))
          ((number? (first name))
           (match-number-length name pattern-rest sent known-values))
          ((equal? howmany '?)
           (longest-match name pattern-rest sent 0 #t known-values))
          ((equal? howmany '!) 
           (longest-match name pattern-rest sent 1 #t known-values))
          ((equal? howmany '*)
           (longest-match name pattern-rest sent 0 #f known-values))
          ((equal? howmany '&)
           (longest-match name pattern-rest sent 1 #f known-values))
          ((null? sent) 'failed)
          ((null? sent) 'failed)
          ((and (equal? howmany '+)
                (number? (first sent)))
           (longest-match name pattern-rest sent 1 #t known-values))
          ((equal? howmany '+) 'failed))))

(define (match-number-length raw-name pattern-rest sent known-values)
  (let ((number (first (extract-number raw-name)))
        (name (butfirst (extract-number raw-name))))
    (match-using-known-values pattern-rest (last (split number sent)) (add raw-name (first (split number sent)) known-values))))

(define (extract-number wd)
  (cond ((empty? wd) '("" ""))
        ((number? (first wd)) (list (word (first (extract-number (butfirst wd))) (first wd)) (last (extract-number (butfirst wd)))))
        (else (list "" wd))))

(define (split num sent)
  (cond ((< (length sent) num) 'failed)
        ((= num 0) (list '() sent))
        (else (let ((part1 (first (split (- num 1) (butfirst sent))))
                    (part2 (last (split (- num 1) (butfirst sent)))))
                (list (sentence (first sent) part1) part2)))))

(define (match-number-value name pattern-rest sent known-values)
  (cond ((number? (first sent)) (match-using-known-values pattern-rest (butfirst sent) (add name (first sent) known-values)))
        (else 'failed)))

(define (length-ok? value howmany)
  (cond ((empty? value) (member? howmany '(? *)))
        ((not (empty? (bf value)))(member? howmany '(* &)))
        (else #t)))

(define (already-known-match value pattern-rest sent known-values)
  (let ((unmatched (chop-leading-substring value sent)))
    (if (not (equal? unmatched 'failed))
        (match-using-known-values pattern-rest unmatched known-values)
        'failed)))

(define (chop-leading-substring value sent)
  (cond ((empty? value) sent)
        ((empty? sent) 'failed)
        ((equal? (first value)(first sent))
         (chop-leading-substring (bf value) (bf sent)))
        (else 'failed)))

(define (longest-match name pattern-rest sent min max-one? known-values)
  (cond ((empty? sent)
         (if (= min 0)
             (match-using-known-values pattern-rest
                                       sent
                                       (add name '() known-values))
             'failed))
        (max-one? 
         (lm-helper name pattern-rest (se (first sent))
                    (bf sent) min known-values))
         (else (lm-helper name pattern-rest
                          sent '() min known-values))))

(define (lm-helper name pattern-rest
                   sent-matched sent-unmatched min known-values)
  (if (< (length sent-matched) min)
      'failed
      (let ((tentative-result (match-using-known-values
                               pattern-rest
                               sent-unmatched
                               (add name sent-matched known-values))))
        (cond ((not (equal? tentative-result 'failed)) tentative-result)
              ((empty? sent-matched) 'failed)
              (else (lm-helper name
                               pattern-rest
                               (bl sent-matched)
                               (se (last sent-matched) sent-unmatched)
                               min
                               known-values))))))

;known values database ADT

;(define (lookup name known-values)
 ; (cond ((empty? known-values) 'no-value)
  ;      ((equal? (first known-values) name)
   ;      (get-value (bf known-values)))
    ;    (else (lookup name (skip-value known-values)))))

(define (lookup name known-values)
  (cond ((empty? known-values) 'no-value)
        ((equal? (first (first known-values)) name) (last (first known-values)))
        (else (lookup name (butfirst known-values)))))

;(define (get-value stuff)
 ; (if (and (equal? (first stuff) '!)
  ;         (or (null? (butfirst stuff))
   ;            (not (equal? (first (butfirst stuff)) '!))))
    ;  '()
     ; (se (first stuff) (get-value (bf stuff)))))

;(define (skip-value stuff)
 ; (if (and (equal? (first stuff) '!)
  ;         (or (null? (butfirst stuff))
   ;            (not (equal? (first (butfirst stuff)) '!))))
    ;  (bf stuff)
     ; (skip-value (bf stuff))))

(define (get-value stuff)
  (last (first stuff)))

(define (skip-value stuff)
  (butfirst stuff))

(define (add name value known-values)
  (if (empty? name)
      known-values
      (append known-values (list (list name value)))))

;16.8: it first checks to see if the placeholder can match to 0 words. If it can, the program will match the placeholder to no words. If not, it returns 'failed.

;16.9 (match '(!one) '(help me out))

;16.10 the program works as expected

;16.11: 5
;16
;4
;12
;14
;8
;when *s come closer to the end of a sentence, it is easier to match because there are less possible lengths for its value. When there are multiple of one named placeholder, that also makes it difficult to match.

;16.12 (match '(*one *two) '(something anything))

;16.13 (match '(*band are !adjective i !verb *band) '(the beatles are cool i love the beatles))

;16.14 if unnamed placeholders were added to the database, unnamed placeholders would have to match other unnamed placeholders of the same type. The paret of the prgram that stops this from happening is (add) which adds the value of the placeholder to the database. If the placeholder has no name, then it is not added to the database.

;16.15 there should not be any case where they need to check for a null list. the lists should always have an ! if they started with any data

;16.16 this way, if it returns #f, you know that it was the second condition that was not met. In the other condition, if the second condition was not met, it would move on to the next part of the cond clause.

;16.17 the program match runs match-using-known-values and establises the empty database

;16.18 1) the first words of the sent must match the stored value
          ; pattern: '(*one hey *two) sentence: '(hey diddle diddle)
      ;2) the partial pattern that remains after the placeholder must match the rest of the sent
          ; pattern: '(*apple banana *apple !grape) sentence: '(apple banana apple grape papaya)
      ;3) the old value must be consistent with the number of words permitted by the howmany part of that placeholder
          ; pattern: '(*stuff and !stuff) sentence: '(hello goodbye and hello)

;16.19 the problem is that ?x is stored in the known-values as '(x ! !). This causes get-value and skip-value to get to the first ! and stop looking for data. I changed them so that they check to make sure that the next character is not also an ! as well. There will still be a problem if you name a place holder with an !, for instance pattern: '(?! is *y !!)

;16.20