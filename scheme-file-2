 (load "~/Documents/Scheme/PLT Scheme v372/berkeley/simply.scm")
(load "~/Documents/Scheme/PLT Scheme v372/berkeley/functions.scm")

(define (even n)
  (if (= n 0)
     #t
  (if (< n 0)
     #f
  (even (- n 2)))))

(define (ex1) '(1 2 7 8 15 234 256.5 1098 1099))
  
(define (elizabeth n)
  (+ 1 n))

(define (hypotenuse a b)
  (sqrt (+ (* a a) (* b b))))

(define (triarea a b)
  (* (* a b) .5))

;(function)


(define (fibonacci n)
  (if (= n 1)
      0
  (if (= n 2)
      1
  (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (fiblist n)
  (if (= n 1)
      (fibonacci n)
  (sentence (fibonacci n) (fiblist (- n 1)))))

(define (fibcount n)
  (if (= n 1)
      0
  (if (= n 2)
      1
  (+ (fibcount (- n 1)) (fibcount (- n 2)) 1))))

(define (fibcountlist n)
  (if (= n 1)
      (fibcount n)
  (sentence (fibcount n) (fibcountlist (- n 1)))))

(define (fibcountdiff n)
  (- (fibcount n) (fibonacci n)))

(define (fibcountdifflist n)
  (if (= n 1)
      (fibcountdiff n)
  (sentence (fibcountdiff n) (fibcountdifflist (- n 1)))))


(define (abc list n)
  (if (= n 1)
      list
  (cdr (abc list (- n 1)))))

(define (nth list n)
  (car (abc list n)))


(define (generic-cmp cmp-fn a b)
  (cmp-fn a b ))

(define (place n list)all
  (if (null? list)
      (cons n list)
  (if (< n (car list))
      (cons n list)
  (if (> n (car list))˜
      (cons (car list) (place n (cdr list)))))))

(define (sort list)
  (if (null? list)
      '()
  (place (car list) (sort (cdr list)))))

(define (genplace cmp-fn n list)
  (if (null? list) 
      (cons n list)
  (if (cmp-fn n (car list))
      (cons n list)
  (cons (car list) (genplace cmp-fn n (cdr list))))))

(define (gensort cmp-fn list)
  (if (null? list)
      '()
  (genplace cmp-fn (car list) (gensort cmp-fn (cdr list)))))

;real excersizes chapter 6

;(define (american-time list)
 ; (cond ((< (car list) 12) (car list))
  ;      ((> (car list) 12) (- (car list) 12))
   ;     ((= (car list) 12) (cond ((= (cdr list) 'pm) 12)
    ;                             ((= (cdr list) 'am) 0)))))

(define (european-time list)
  (cond ((equal? (cdr list) '(pm)) (+ (car list) (12)))
        ((equal? (cdr list) '(am)) (car list))))

(define (european-time list)
  (cond ((equal? (cdr list) '(pm)) (cond ((= (car list) 12) 12)
                                         (else (+ (car list) 12))))
        ((equal? (cdr list) '(am)) (cond ((= (car list) 12) 0)
                                         (else (car list))))))

(define (american-time n)
  (cond ((< n 12) (cons n '(am)))
        ((= n 12) (cons n '(pm)))
        ((> n 12) (cons (- n 12) '(pm)))
        ((= n 0) '(12 am))
        ((= n 24) '(12 am))))

(define (length-less a b)
  (< (length a) (length b)))

(define ttt '( ( 1 2) (0 ) () ( 1 2 3 4) (1 1) (1 2 3 4 5 6)))

(define (even-is-better a b)
  (cond
    ((even a) #t)
    ((even b) #f)
    (#t #t)))

(define ttt2 '( 1 3 4 6 8 7 3 90 7 2 68 4))

(define (equalnine? a b)
  (= 9 (+ a b)))

(define (equalninesort list)
  (cond ((null? list) '())
        ((null? (cdr list)) '())
        ((= (+ (car list) (car (cdr list))) 9) (cons (cons (car list) (cons (car (cdr list)) '()))
                                               (equalninesort (cdr (cdr list)))))
        (#t (equalninesort (cdr list)))))

(define (makepairs cmp-fn n list)
  (cond ((null? list) '())
        ((cmp-fn n (car list)) 
         (cons (cons n (cons (car list) ())) (makepairs cmp-fn n (cdr list))))
        (#t (makepairs cmp-fn n (cdr list)))))

(define (allposscomb cmp-fn list)
  (cond ((null? list) ())
        (#t (append (makepairs cmp-fn (car list) (cdr list)) (allposscomb cmp-fn (cdr list))))))

(define (productsixtyfour? a b)
  (= (* a b) 64))

(define (makelist n)
  (cond ((= n 0) (cons 0 ()))
        ((> n 0) (cons n (makelist (- n 1))))))

(define (divseven? n)
  (cond ((< n 0) #f)
        ((= n 0) #t)
        ((= (- n 7) 0) #t)
        ((< (- n 7) 0) #f)
        ((> (- n 7) 0) (divseven? (- n 7)))))

(define (adddivseven? a b)
  (divseven? (+ a b)))

(define (increment-by n)
  (lambda (a) (+ n a)))

(define (makepairsuniq cmp-fn n list)
  (cond ((null? list) '())
        ((cmp-fn n (car list)) (makepairsuniq cmp-fn n (cdr list)))
        (#t (cons (car list) (makepairsuniq cmp-fn n (cdr list))))))

(define (uniq cmp-fn list)
  (cond ((null? list) '())
        (#t (cons (car list) (uniq cmp-fn (makepairsuniq cmp-fn (car list) (cdr list)))))))

(define (exlist2)
  '(3 5 2 8 6 9 3 6 7 5 2 0 1 9))

;more chapter 6 excercizes

(define (teen? n)
  (cond ((< n 20) (cond ((> n 12) #t)
                        (#t #f)))
        (#t #f)))

(define (indef-article word)
  (cond ((vowel? (first word)) (cons 'an (cons word ())))
        (#t (cons 'a (cons word ())))))

(define (vowel? n)
  (member? n '(a e i o u)))

(define (plural2 wd)
  (cond ((equal? (last wd) 'y) (word (butlast wd) ('ies)))
        ((equal? (last wd) 's) (word (butlast wd) ('es)))
        (#t (word wd 's))))

(define (thismany n wd)
  (cond ((= 1 n) (cons n (cons wd ())))
        (#t (cons n (cons (plural wd) ())))))

(define (sort2 list)
  (cond ((< (car list) (car (cdr list))) list)
        (#t (cons (car (cdr list)) (cons (car list) ())))))

(define (valid-date? a b c)
  (cond ((> a 12) #f)
        ((< a 1) #f)
        ((< b 1) #f)
        ((= a 2) (cond ((divby4? c) (cond ((divby100? c) (cond ((divby400? c) (cond ((< b 30) #t)
                                                                                    ((> b 29) #f)))
                                                               (#t (cond ((< b 29) #t)
                                                                         ((> b 28) #f)))))
                                          (#t (cond ((< b 30) #t)
                                                    ((> b 29) #f)))))
                       (#t (cond ((< b 29) #t)
                                 ((> b 28) #f)))))
        ((member? a '(1 3 5 7 8 10 12)) (cond ((< b 32) #t)
                                              ((> b 31) #f)))
        (#t (cond ((< b 31) #t)
                  ((> b 30) #f)))))

(define (divby4? n)
  (if (= n 0)
     #t
  (if (< n 0)
     #f
  (divby4? (- n 4)))))

(define (divby100? n)
  (if (= n 0)
     #t
  (if (< n 0)
     #f
  (divby100? (- n 100)))))

(define (divby400? n)
  (if (= n 0)
     #t
  (if (< n 0)
     #f
  (divby400? (- n 400)))))

;a beautiful plural program that does a bunch of weird stuff

(define (plural wd)
  (cond ((member? wd '(bison buffalo cod deer fish moose pike plankton salmon sheep squid swine trout aircraft watercraft hovercraft counsel)) wd)
        ((equal? wd 'ox) 'oxen)
        ((equal? wd 'child) 'children)
        ((equal? wd 'brother) 'brethren)
        ((equal? wd 'bus) 'buses)
        ((equal? wd 'octopus) 'octopoda)
        ((equal? (last wd) 'y) (cond ((vowel? (last (butlast wd))) (word wd 's))
                                     (#t (word (butlast wd)) (word (butlast wd) 'ies))))
        ((equal? (last wd) 's) (cond ((equal? (last (butlast wd)) 'e) (cond ((equal? (last (butlast (butlast wd))) 'i) wd)
                                                                           (#t (word (butlast wd) 'es))))
                                     ((equal? (last (butlast wd)) 'i) (word (butlast (butlast wd)) 'es))
                                     ((equal? (last (butlast wd)) 'u) (word (butlast (butlast wd)) 'i))
                                     (#t (word  wd 'es))))
        ((equal? (last wd) 'x) (cond ((member? (last (butlast wd)) '(i e)) (word (butlast (butlast (butlast wd))) 'ces))
                                     (#t (word (butlast wd) 'es))))
        ((equal? (last wd) 'o) (word wd 'es))
        ((equal? (last wd) 'h) (cond ((equal? (last (butlast wd)) 't) (word wd 's))
                                     (#t (word wd 'es))))
        ((equal? (last wd) 'f) (cond ((equal? (last (butlast wd)) 'f) (word (butlast (butlast wd)) 'ves)) 
                                     (#t (word (butlast wd) 'ves))))
        ((equal? (last wd) 'e) (cond ((equal? (last (butlast wd)) 'f) (word (butlast (butlast wd)) 'ves))
                                     (#t (word wd 's))))
        ((equal? (last wd) 'a) (cond ((vowel? (last (butlast wd))) (word wd 's))
                                     ((equal? (last (butlast wd)) 'm) (word wd 'ta))
                                     (#t (word wd 'e))))
        ((equal? (last wd) 'm) (cond ((equal? (last (butlast wd)) 'u) (word (butlast (butlast wd)) 'a))
                                     (#t (word wd 's))))
        ((equal? (last wd) 'n) (cond ((equal? (last (butlast wd)) 'o) (word (butlast (butlast wd)) 'a))
                                     ((equal? (last (butlast wd)) 'a) (word (butlast (butlast wd)) 'en))
                                     (#t (word wd 's))))
        ((equal? (last wd) 'u) (cond ((equal? (last (butlast wd)) 'a) (cond ((equal? (last (butlast (butlast wd))) 'e) (word wd 'x))
                                                                             (#t (word wd 's))))
                                     (#t (word wd 's))))
        ((equal? (last wd) 'z) (word wd 'zes))
        (#t (word wd 's))))

(define (greet list)
  (cond ((null? list) '(who?))
        ((member? (last list) '(jr sr junior senior)) (greet (butlast list)))
        ((member? (car list) '(queen king princess prince)) '(it is my honor your majesty))
        ((member? (car list) '(dr professor doctor)) (cons 'hello (cons (car list) (cons (last list) () ))))
        ((equal? list '(stanley livingston)) '(dr livingston i presume?))
        ((equal? (last list) 'skywalker) (append '(may the force be with you) (cons (car list) ())))
        ((equal? list '(vaidehi srinivas)) '(welcome master))
        ((equal? list '(vishnu srinivas)) '(greetings poopface))
        (#t (cons 'hello (cons (car list) ())))))

(define (describe-time n)
  (cond ((word? n) (describe-time (append n '(seconds))))
        ((equal? (last n) 'seconds) (cond ((< (car n) 60) n)
                                             (#t (describe-time (cons (/ (car n) 60) '(minutes))))))
        ((equal? (last n) 'minutes) (cond ((< (car n) 60) n)
                                             (#t (describe-time (cons (/ (car n) 60) '(hours))))))
        ((equal? (last n) 'hours) (cond ((< (car n) 24) n)
                                           (#t (describe-time (cons (/ (car n) 24) '(days))))))
        ((equal? (last n) 'days) (cond ((< (car n) 365) n)
                                          (#t (describe-time (cons (/ (car n) 365) '(years))))))
        ((equal? (last n) 'years) (cond ((< (car n) 100) n)
                                           (#t (describe-time (cons (/ (car list) 100) '(centuries))))))
        ((equal? (last n) 'centuries) (cond ((< (car n) 10) n)
                                               (#t (cons (/ (car n) 10) '(millenia)))))))

(define (apologize)
  '(i am so sorry for what i did, please forgive me))

;chapter 7

(define (double sent)
  (se sent sent))
;; answers for 7.1: 'aeiou , () , 6713094217 , #f , (16, 144, 0) , 'ai , 25, (go d sunshi)
;; 7.2 keep , every , first , (accumulate (every last)) , every 

;7.4
(define (choose-beatles cmp-fn)
  (keep cmp-fn '(john paul george ringo)))

(define (ends-vowel? wd) (vowel? (last wd)))

;7.5
(define (transform-beatles cmp-fn)
  (every cmp-fn '(john paul george ringo)))

(define (amazify name)
  (word 'the-amazing- name))

;7.6
(define (words word)
  (every phonalph word))

(define (phonalph n)
  (cond ((equal? n 'a) 'alpha)
        ((equal? n 'b) 'bravo)
        ((equal? n 'c) 'charlie)
        ((equal? n 'd) 'delta)
        ((equal? n 'e) 'echo)
        ((equal? n 'f) 'foxtrot)
        ((equal? n 'g) 'golf)
        ((equal? n 'h) 'hotel)
        ((equal? n 'i) 'india)
        ((equal? n 'j) 'juliett)
        ((equal? n 'k) 'kilo)
        ((equal? n 'l) 'lima)
        ((equal? n 'm) 'mike)
        ((equal? n 'n) 'november)
        ((equal? n 'o) 'oscar)
        ((equal? n 'p) 'papa)
        ((equal? n 'q) 'quebec)
        ((equal? n 'r) 'romeo)
        ((equal? n 's) 'sierra)
        ((equal? n 't) 'tango)
        ((equal? n 'u) 'uniform)
        ((equal? n 'v) 'victor)
        ((equal? n 'w) 'whiskey)
        ((equal? n 'x) 'xray)
        ((equal? n 'y) 'yankee)
        ((equal? n 'z) 'zulu)
        ((equal? n '1) 'one)
        ((equal? n '2) 'two)
        ((equal? n '3) 'three)
        ((equal? n '4) 'four)
        ((equal? n '5) 'five)
        ((equal? n '6) 'six)
        ((equal? n '7) 'seven)
        ((equal? n '8) 'eight)
        ((equal? n '9) 'nine)
        ((equal? n '0) 'zero)
        (#t 'huh? )))

;7.7

(define (letter-count sent)
  (accumulate + (every lttr-cnt sent)))

(define (lttr-cnt wd)
  (accumulate + (every always-one wd)))

(define (always-one n)
  (cond (#t 1)))

;7.8
(define (exaggerate sent)
  (every exag sent))

(define (exag wd)
  (cond ((number? wd) (* 2 wd))
        ((equal? wd 'good) 'great)
        ((equal? wd 'bad) 'terrible)
        (#t wd)))

;7.9

(define (every-same sent)
  sent)
  
(define (keep-same sent)
  #t)

; accumulate-same is just the function (sentence)

;7.10

(define (true-for-all? cmp-fn list)
  (cond ((null? list) #t)
        ((cmp-fn (car list)) (true-for-all? cmp-fn (cdr list)))
        (#t #f)))

;7.11

(define (base-grade n)
  (cond ((equal? n 'a) 4)
        ((equal? n 'b) 3)
        ((equal? n 'c) 2)
        ((equal? n 'd) 1)
        ((equal? n 'f) 0)))

(define (grade-modifier wd)
  (cond ((equal? (last wd) '+) (+ (base-grade (first wd)) '.33))
        ((equal? (last wd) '-) (+ (base-grade (first wd)) '-.33))
        ((member? (last wd) '(a b c d f)) (base-grade wd))))

(define (gpa list)
  (cond ((null? list) '(enter grades))
        (#t (/ (accumulate + (every grade-modifier list)) (length list)))))

;7.12

(define (count-ums sent)
  (accumulate + (every cnt-ums sent)))
  
(define (cnt-ums wd)
  (cond ((equal? wd 'um) 1)
        (#t 0)))

;7.13

(define (unspell-letter n)
  (cond ((member? n '(a b c)) 2)
        ((member? n '(d e f)) 3)
        ((member? n '(g h i)) 4)
        ((member? n '(j k l)) 5)
        ((member? n '(m n o)) 6)
        ((member? n '(p q r s)) 7)
        ((member? n '(t u v)) 8)
        ((member? n '(w x y z)) 9)))

(define (phone-unspell wd)
  (every unspell-letter wd))

;7.14
(define (subword wd a b)
  ((repeated butlast (- (accumulate + (every always-one wd)) b)) ((repeated butfirst (- a 1)) wd)))



;chapter 8
;8.1 #<procedure> , 34 , (yan etim ta lal) , function expects one arg

;8.2 ?

(define second (lambda (stuff)(first (bf stuff))))

(define make-adder (lambda (num) (lambda (x) (+ num x))))


;8.3 returns the last word of the sentence

;8.4 
(define (prepend-every n sent)
  (every (lambda (wd) (word n wd)) sent))

;8.5
(define (sentence-version cmp-fn)
  (lambda (list) (every cmp-fn list)))

(define (square n)
  (* n n))

;8.6
(define (letterwords x sent)
  (keep (lambda (word) (member? x word)) sent))

;8.7
(define (hang wd guesses)
  (accumulate word (every (lambda (letter) (if (member? letter guesses)
                              letter
                              '_))
         wd)))

;8.8
(define (common-words a b)
  (keep (lambda (word) (member? word a)) b))

;8.9 
(define (appearances elem sent)
  (accumulate + (every (lambda (x) (cond ((equal? elem x) 1)
                           (else 0))) sent)))

;8.10
(define (unabbrev a b)
  (every 
   (lambda (n) 
           (cond ((number? n) (first ((repeated butfirst (- n 1)) b)))
                 (#t n))) 
     a))

;8.11
(define (first-last sent)
  (keep (lambda (n) (equal? (first n) (last n))) sent))

;8.12
(define (compose cmp-fn1 cmp-fn2)
  (lambda (x) (cmp-fn1 (cmp-fn2 x))))

;8.13
(define (substitute a b sent)
  (every (lambda (wd) (cond ((equal? wd b) a)
                            (#t wd))) sent))

;8.14
(define (type-check f pred)
  (lambda (arg) (cond ((pred arg) (f arg))
                      (#t #f))))

;8.15
(define (aplize fn)
  (lambda (n) (cond ((list? n) (every fn n))
                    (else (fn n)))))

;8.16
(define (keep2 pred-fn sent)
  (accumulate sentence
              (every 
               (lambda (wd) (cond ((pred-fn wd) wd)
                                  (#t ()))) 
               sent)))

;chapter 9: tic tac toe

(define (ttt position me)
  (let ((triples (find-triples position)))
  (cond ((already-won? triples))
        ((tie-game2? triples position me))
        ((i-can-win? triples me))
        ((opponent-can-win? triples me))
        ((i-can-fork? triples me))
        ((i-can-advance? triples me))
        (else (best-free-square triples)))))

(define (find-triples position)
  (every (lambda (x) (substitute-triple x position)) '(123 456 789 147 258 369 159 357)))

(define (substitute-triple triple position)
  (accumulate word (every (lambda (x) (substitute-letter x position)) triple)))

(define (substitute-letter n position)
  (cond ((member? (substitute-letter-real n position) '(x o)) (substitute-letter-real n position))
        ((equal? (substitute-letter-real n position) '_) n)))

(define (substitute-letter-real n position)
  (first ((repeated butfirst (- n 1)) position)))

;i-can-win
(define (my-pair? triple me)
  (and (= (appearances me triple) 2)
       (= (appearances (opponent me) triple) 0)))

(define (opponent me)
  (cond ((equal? me 'x) 'o)
        (else 'x)))

(define (i-can-win? triples me)
  (cond ((null? (keep (lambda (x) (my-pair? x me)) triples)) #f)
        (else (choose-win (keep (lambda (x) (my-pair? x me)) triples)))))

(define (i-can-win2? triples me)
    (cond ((null? (keep (lambda (x) (my-pair? x me)) triples)) #f)
        (else #t)))

(define (choose-win winning-triples)
  (cond ((null? winning-triples) #f)
        (else (keep number? (first winning-triples)))))



;opponent-can-win

(define (opponent-can-win? triples me)
  (i-can-win? triples (opponent me)))

;i-can-win-next-time?

(define (pivots triples me)
  (repeated-numbers (keep (lambda (triple) (my-single? triple me))
                          triples)))

(define (i-can-fork? triples me)
  (first-if-any (pivots triples me)))

(define (first-if-any sent)
  (cond ((null? sent) #f)
        (else (car sent))))

(define (my-single? triple me)
  (and (= 0 (appearances (opponent me) triple))
       (= 1 (appearances me triple))))

(define (extract-digit n wd)
  (keep (lambda (x) (cond ((equal? n x) #t)
                          (else #f))) wd))

(define (sort-digits number-word)
  (every (lambda (x) (extract-digit x number-word)) '(1 2 3 4 5 6 7 8 9)))

(define (repeated-numbers sent)
  (every first 
         (keep (lambda (wd) (>= (count wd) 2))
               (sort-digits (accumulate word sent)))))


;i-can-advance
(define (i-can-advance? triples me)
  (best-move (keep (lambda (triple) (my-single? triple me)) triples)
             triples
             me))

(define (best-move my-triples all-triples me)
  (if (empty? my-triples)
      #f
      (best-square (first my-triples) all-triples me)))

(define (best-square my-triple triples me)
  (best-square-helper (pivots triples (opponent me))
                      (keep number? my-triple)))

(define (best-square helper opponent-pivots pair)
  (if (member? (first pair) opponent-pivots)
      (first pair)
      (last pair)))

(define (best-free-square triples)
  (first-choice (accumulate word triples) '(5 1 3 7 9 2 4 6 8)))

(define (first-choice possibilities preferences)
  (first (keep (lambda (square) (member? square possibilities))
               preferences)))
;9.1
(define (already-won? triples)
  (cond ((member? 'xxx triples) '(x has won the game))
        ((member? 'ooo triples) '(o has won the game))
        (else #f)))

;9.2
(define (tie-game? triples)
  (equal? 8 (length (keep filled-triple triples))))

(define (filled-triple triple)
  (cond ((number? (first triple)) #f)
        ((number? (first (butfirst triple))) #f)
        ((number? (last triple)) #f)
        (else #t)))

;9.3
(define (tie-game2? triples position me)
  (cond ((equal? 1 (appearances '_ position)) (ttt (every (lambda (triple) (every (lambda (digit) (cond ((number? digit) me)
                                                                                                        (else digit))))) triples) me ))
        (else #f)))

;9.4


;chapter 10

;10.1
(define (gertrude wd)
  (let ((rosalinda (if (vowel? (first wd)) 'an 'a)))
  (se rosalinda wd 'is rosalinda wd 'is rosalinda wd)))

;10.2
;(let ((pi 3.14159)
;      (pie '(lemon merangue)))
;  (se '(pi is) pi '(but pie is) pie))

;10.3
(define (who sent)
  (every (lambda (wd) (describe wd sent)) '(pete roger john keith)))

(define (describe person sent)
  (se person sent))

;10.4
(define (superlative adjective wd)
    (se (word adjective 'est) wd))

;10.5 in the let of thi program, + becomes multiplication and * becomes addition. this works because in the let, the + has not yet

;chapter 11

(define (length-word wd)
  (accumulate + (every always-one wd)))

(define (flip list)
  (cond ((null? list) ())
        (else (se (last list) (flip (butlast list))))))

;11.1
(define (downup wd)
  (cond ((list? wd) (cond ((equal? 1 (length-word (last wd))) (se wd (flip (butlast wd))))
                          (else (downup (se wd (butlast (last wd)))))))
        ((word? wd) (downup (se wd (butlast wd))))
        (else '(invalid input))))

(define (explode wd)
  (cond ((= (length-word wd) 1) wd)
        (else (sentence (first wd) (explode (butfirst wd))))))

(define (letter-pairs wd)
  (cond ((= (length-word wd) 2) wd)
        (else (se (word (first wd) (first (butfirst wd))) (letter-pairs (butfirst wd))))))

;11.2
(define (count-ums sent)
  (cond ((null? sent) 0)
        ((equal? (first sent) 'um) (+ 1 (count-ums (butfirst sent))))
        (else (count-ums (butfirst sent)))))

;11.3
(define (phone-unspell2 wd)
  (cond ((= (length-word wd) 1) (unspell-letter wd))
        (else (word (unspell-letter (first wd)) (phone-unspell2 (butfirst wd))))))

;11.4 who cares?

;11.5
(define (initials sent)
  (cond ((null? sent) ())
        (else (sentence (first (first sent)) (initials (butfirst sent))))))

;11.6
(define (countdown n)
  (cond ((= n 0) '(blastoff!))
        (else (cons n (countdown (- n 1))))))

;11.7
(define (copies n wd)
  (cond ((= n 1) (cons wd ()))
        (else (cons wd (copies (- n 1) wd)))))

;chapter 12
(define (reverse wd)
  (cond ((= 1 (length-word wd)) wd)
        (else (word (last wd) (reverse (butlast wd))))))

(define (evens-only sent)
  (cond ((= 1 (length sent)) ())
        ((= 2 (length sent)) (cons (last sent) ()))
        (else (cons (first (butfirst sent)) (evens-only (butfirst (butfirst sent)))))))

;12.1 
(define (addup nums)
  (if (null? nums)
      0
      (+ (first nums) (addup (bf nums)))))

;12.2
(define (acronym sent)
  (if (= (count sent) 1)
      (first (first sent))
      (word (first (first sent))
            (acronym (bf sent)))))

;12.3 no -1 does not follow the pattern

;12.4  it writes a sentence backwards
(define (f sent)
  (cond ((null? sent) sent)
        (else (sentence (f (butfirst sent)) (first sent)))))


;12.5
(define (exaggerate2 sent)
  (cond ((null? sent) ())
        (else (cond ((number? (first sent)) (cons (* n 2) (exaggerate2 (butfirst sent))))
                    ((equal? (first sent) 'good) (cons 'great (exaggerate2 (butfirst sent))))
                    ((equal? (first sent) 'bad) (cons 'terrible (exaggerate2 (butfirst sent))))
                    (else (cons (first sent) (exaggerate2 (butfirst sent))))))))

;12.6
(define (gpa2 total list count)
  (cond ((null? list) (/ total count))
        (else (gpa2 (+ total (grade-modifier (first list))) (butfirst list) (+ 1 count)))))

;12.7
(define (spell-digit digit)
  (item (+ 1 digit)
        '(zero one two three four five six seven eight nine)))

(define (spell-number number)
  (cond ((= (length-word number) 1) (spell-digit number))
        (else (sentence (spell-digit (first number)) (spell-number (butfirst number))))))

;12.8
(define (numbers-only sent)
  (cond ((null? sent) ())
        ((number? (first sent)) (cons (first sent) (numbers-only (butfirst sent))))
        (else (numbers-only (butfirst sent)))))

;12.9
(define (real-words-only sent)
  (cond ((null? sent) ())
        ((real-word? (first sent)) (cons (first sent) (real-words-only (butfirst sent))))
        (else (real-words-only (butfirst sent)))))

(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

;12.10
(define (remove wd sent)
  (cond ((null? sent) ())
        ((equal? wd (first sent)) (remove wd (butfirst sent)))
        (else (cons (first sent) (remove wd (butfirst sent))))))

;12.11
(define (count-all n)
  (cond ((empty? n) 0)
        (else (+ 1 (count-all (butfirst n))))))

;12.12
(define (arabic roman)
  (cond ((equal? roman "") 0)
        ((= (count roman) 1) (convert-to-arabic roman))
        ((< (convert-to-arabic (first roman)) (convert-to-arabic (first (butfirst roman)))) (+ (arabic (butfirst (butfirst roman))) (- (convert-to-arabic (first (butfirst roman)))(convert-to-arabic (first roman)))))
        (else (+ (convert-to-arabic (first roman)) (arabic (butfirst roman))))))
        
(define (convert-to-arabic roman)
  (butfirst (first (keep (lambda (wd) (equal? roman (first wd))) '(i1 v5 x10 l50 c100 d500 m1000)))))

;REVISIT 12.13 6/25
(define time-unit-list '((60 seconds)(60 minutes)(24 hours)(7 days)(52 weeks)(100 years)(10 centuries)(infinity millenia)))

(define volume-unit-list '((3 teaspoons) (16 tablespoons) (2 cups) (2 pints) (4 quarts) (infinity gallons)))

(define length-unit-list '((12 inches) (3 feet) (22 yards) (10 chains) (8 furlongs) (infinity miles)))

(define (round-down n)
  (cond ((<= (round n) n) (round n))
        (else (round (- n 1)))))

(define (quotient a b)
  (round-down (/ a b)))

(define (leftover n)
  (- n (round-down n)))

(define (aaa number list)
  (cond ((= number 0) ())
        ((equal? (first (first list)) 'infinity) (sentence number (butfirst (first list))))
        ((< number (first (first list))) (sentence number (butfirst (first list))))
        (else (sentence 
               (aaa (quotient number (first (first list))) (butfirst list)) 
               (sentence (remainder number (first (first list))) (butfirst (first list)))) )))

(define (describe-time2 secs)
  (aaa secs time-unit-list))

;chapter 14
(define (disjoint-pairs wd)
  (cond ((= (length-word wd) 1) (cons wd ()))
        ((= (length-word wd) 2) (cons wd ()))
        (else (cons (word (first wd) (first (butfirst wd))) (disjoint-pairs (butfirst (butfirst wd)))))))

;14.1
(define (remove-once wd sent)
  (cond ((null? sent) ())
        ((equal? (first sent) wd) (butfirst sent))
        (else (cons (first sent) (remove-once wd (butfirst sent))))))
;14.2
(define (up wd)
  (cond ((= (length-word wd) 1) (cons wd ()))
        (else (sentence (up (butlast wd)) wd))))

;14.3
(define (remdup sent)
  (cond ((null? sent) ())
        (else (add-if-not-present (first sent) (remdup (butfirst sent))))))

(define (add-if-not-present wd sent)
  (cond ((member? wd sent) sent)
        (else (cons wd sent))))

;14.4
(define (odds sent)
  (cond ((null? sent) ())
        ((= 1 (length sent)) sent)
        (else (cons (first sent) (odds (butfirst (butfirst sent)))))))

;14.5
(define (letter-count2 sent)
  (cond ((null? sent) 0)
        (else (+ (count (first sent)) (letter-count2 (butfirst sent))))))

;14.6
(define (member2? wd sent)
  (cond ((null? sent) #f)
        ((equal? wd (first sent)) #t)
        (else (member2? wd (butfirst sent)))))

;14.7
(define (differences list)
  (cond ((= 1 (length list)) ())
        (else (cons (- (first (butfirst list)) (first list)) (differences (butfirst list))))))

;REVISIT 14.8 ?? 6/25

(define (make-list x)
  (cond ((word? x) (cons x ()))
        (else x)))

(define (make-sentence number x)
  (cond ((= number 0) ())
        (else (cons x (make-sentence (- number 1) x)))))

(define (expand list) 
  (cond ((null? list) ())
        ((number? (first list)) (sentence (make-sentence (first list) (first (butfirst list))) (expand (butfirst (butfirst list)))))
        (else (cons (first list) (expand (butfirst list))))))

;14.9 ***
(define (location wd sent)
  (cond ((null? sent) #f)
        ((equal? wd (first sent)) 1)
        (else (+ 1 (location wd (butfirst sent))))))

;14.10
(define (count-adjacent-duplicates sent)
  (cond ((null? (butfirst list)) 0)
        ((equal? (first sent) (first (butfirst sent))) (+ 1 (count-adjacent-duplicates (butfirst sent))))
        (else  (count-adjacent-duplicates (butfirst sent)))))

;14.11
(define(remove-adjacent-duplicates sent)
  (cond ((null? (butfirst list)) sent)
        ((equal? (first sent) (first (butfirst sent))) (remove-adjacent-duplicates (butfirst sent)))
        (else (cons (first sent) (remove-adjacent-duplicates (butfirst sent))))))

;14.12
(define (progressive-squares? list)
  (cond ((null? (butfirst list)) #t)
        ((= (first (butfirst list)) (* (first list) (first list))) (progressive-squares? (butfirst list)))
        (else #f)))

;14.13
(define (pigl3 wd)
  (word (pigl3-help wd 0) 'ay))

(define (pigl3-help wd n)
  (cond ((empty? wd) wd)
        ((vowel? (first wd)) wd)
        ((= n (count wd)) wd)
        (else (pigl3-help (word (butfirst wd) (first wd)) (+ n 1)))))

;14.14
(define (same-shape? sent1 sent2)
  (cond ((and (null? sent1)
              (null? sent2)) #t)
        ((or (null? sent1)
             (null? sent2)) #f)
        ((= (count (first sent1)) (count (first sent2))) (same-shape? (butfirst sent1) (butfirst sent2)))
        (else #f)))

;14.15
(define (merge sent1 sent2)
  (cond ((null? sent1) sent2)
        ((null? sent2) sent1)
        ((< (first sent1) (first sent2)) (cons (first sent1) (merge (butfirst sent1) sent2)))
        (else (cons (first sent2) (merge sent1 (butfirst sent2))))))

;14.16
(define (vowel2? letter)
  (member? letter '(a e i o u y)))

(define (syllables wd)
  (syllables-help wd #f))

(define (syllables-help wd prevwl)
  (cond ((empty? wd) 0)
        ((vowel? (first wd)) (+ (if prevwl 0 1)(syllables-help (butfirst wd) #t)))
        (else (syllables-help (butfirst wd) #f))))

;15.1
(define (to-binary n)
  (cond ((= n 0) "")
        ((even? n) (word (to-binary (/ n 2)) 0))
        (else (word (to-binary (/ (- n 1) 2)) 1))))

;15.2
(define (palindrome? sent)
  (cond ((= (count (accumulate word sent)) 0) #t)
        ((= (count (accumulate word sent)) 1) #t)
        ((equal? (first (accumulate word sent)) (last (accumulate word sent))) (palindrome? (butfirst (butlast (accumulate word sent)))))
        (else #f)))

;15.3
(define (substrings wd)
  (cond ((empty? wd) '())
        (else (remove-repeat (append (cons wd ()) (substrings (butfirst wd)) (substrings (butlast wd)))))))

(define (remove-repeat list)
  (cond ((null? list) list)
        ((member? (car list) (cdr list)) (remove-repeat (cdr list)))
        (else (cons (car list) (remove-repeat (cdr list))))))

;15.4
(define (substring? subs wd)
  (member? subs (substrings wd)))

(define (lead-match subs wd)
  (cond ((empty? subs) #t)
        ((empty? wd) #f)
        ((equal? (first subs) (first wd)) (lead-match (butfirst subs) (butfirst wd)))
        (else #f)))

(define (substring2? subs wd)
  (cond ((empty? subs) #t)
        ((empty? wd) #f)
        ((lead-match subs wd) #t)
        (else (substring2? subs (butfirst wd)))))

;15.5
(define phone-letters-lst '((2 a b c) (3 d e f) (4 g h i) (5 j k l) (6 m n o) (7 p q r s) (8 t u v) (9 w x y z) (1 1) (0 0)))

(define (phone-letters digit list)
  (cond ((equal? (first (first list)) digit) (cdr (first list)))
        (else (phone-letters digit (cdr list)))))

(define (every-add-letters1 letter words)
  (every (lambda (x) (word letter x)) words))

(define (every-add-letters2 letters words)
  (every (lambda (x) (every-add-letters1 x words)) letters))

(define (phone-spell num)
  (cond ((= (count num) 1) (phone-letters num phone-letters-lst))
        (else (every-add-letters2 (phone-letters (first num) phone-letters-lst) (phone-spell (butfirst num))))))

;15.6
;rat cat dog boy girl saw owned chased bit 

(define (unscramble list)
  (unscramble-help (remove-words list bad-words)))

(define (remove-words list wrdstormve)
  (cond ((null? wrdstormve) list)
        (else (remove-words (remove-word (first wrdstormve) list) (butfirst wrdstormve)))))

(define bad-words '(this is the that))

(define (remove-word word list)
  (cond ((null? list) list)
        ((equal? word (first list)) (remove-word word (cdr list)))
        (else (cons (first list) (remove-word word (cdr list))))))

(define (unscramble-help list)
  (cond ((= (count list) 1) (append '(this is the) list))
        (else (append (unscramble-help (butfirst (butlast list))) (cons 'that (cons (last list) (cons 'the (cons (first list) ()))))))))

;project: scoring poker hands

(define (suit hand)
  (let ((cards (every first hand)))
    (cond ((= (count hand) 1) (trnslte (first (first hand)) *translating-list*))
          ((equal? (first cards) (first (butfirst cards))) (suit (butfirst hand)))
          (else #f))))

(define *translating-list* '((d diamonds) (h hearts) (c clubs) (s spades) (a aces) (1 ones) (2 twos) (3 threes) (4 fours) (5 fives) (6 sixes) (7 sevens) (8 eights) (9 nines) (10 tens) (j jacks) (q queens) (k kings) (- wild)))

;(define *list-of-ranks* '(a 2 3 4 5 6 7 8 9 10 j q k a))

(define *list-of-ranks* '(a k q j 10 9 8 7 6 5 4 3 2 a))

(define (trnslte symbol list)
  (cond ((equal? (first (first list)) symbol) (butfirst (first list)))
        (else (trnslte symbol (butfirst list)))))

(define (count-rank rank ranks)
  (cond ((null? ranks) 0)
        ((equal? rank (first ranks)) (+ 1 (count-rank rank (cdr ranks))))
        (else (count-rank rank (cdr ranks)))))

(define (rank-info cards)
  (every (lambda (x) (count-rank x (every butfirst cards))) *list-of-ranks*))

(define (straight? cards)
  (straight-help? (rank-info cards) 0 (cons 'pad *list-of-ranks*)))

(define (straight-help? hand-info number list)
  (cond ((= number 5) (trnslte (first list) *translating-list*))
        ((null? hand-info) #f)
        ((< 0 (first hand-info)) (straight-help? (butfirst hand-info) (+ 1 number) (butfirst list)))
        (else (straight-help? (butfirst hand-info) 0 (butfirst list)))))

(define (poker-value hand)
  (cond ((and (suit hand)
              (royal-straight? hand)) '(royal flush))
        ((and (suit hand)
              (straight? hand)) '(straight flush))
        ((member? 4 (rank-info hand)) '(four of a kind))
        ((and (member? 3 (rank-info hand))
              (member? 2 (rank-info hand))) '(full house))
        ((straight? hand) '(straight))
        ((member? 3 (rank-info hand)) '(three of a kind))
        ((two-pair? hand) '(two pair))
        ((member? 2 (rank-info hand)) '(pair))
        (else '(nothing))))

(define (rank-name-help num hand-info list)
  (cond ((null? hand-info) ())
        ((equal? num (first hand-info)) (append (trnslte (first list) *translating-list*) (rank-name-help num (butfirst hand-info) (butfirst list))))
        (else (rank-name-help num (butfirst hand-info) (butfirst list)))))

(define (rank-name num hand-info)
  (rank-name-help num hand-info *list-of-ranks*))

(define (poker-value2 hand)
  (let ((suit-name (suit hand)) (r-i (rank-info hand)))
    (cond ((and suit-name  (equal? (straight? hand) '(aces)) (append '(royal flush --)  suit-name))) 
          ((and suit-name (straight? hand)) (append '(straight flush of)(list suit-name)))
          ((member? 4 r-i) (append '(four of a kind --) (rank-name 4 r-i)))
          ((and (member? 3 (rank-info hand))
              (member? 2 (rank-info hand))) (append '(full house --) (rank-name 3 r-i) '(over) (rank-name 2 r-i)))
          (suit-name (append '(flush --) suit-name))
          ((straight? hand) (append '(straight --) (straight? hand) '(high)))
          ((member? 3 (rank-info hand)) (append '(three of a kind --) (rank-name 3 r-i)))
          ((= 2 (count (rank-name 2 r-i))) (append '(two pair --) (rank-name 2 r-i)))
          ((member? 2 (rank-info hand)) (append '(pair --) (rank-name 2 r-i)))
          (else '(nothing)))))

(define (remove-from-list x list)
  (cond ((null? list) list)
        ((equal? x (first list)) (butfirst list))
        (else (cons (first list) (remove-from-list x (butfirst list))))))

;(define (leave-one-out-help list1 list2)
 ; (cond ((null? (cdr list2)) (list (remove-from-list (first list2) list1)))
 ;       (else (cons (remove-from-list (first list2) list1) (leave-one-out-help list1 (butfirst list2))))))

;(define (leave-one-out list)
 ; (leave-one-out-help list list))
;
;(define (make-every-combo list num)
 ; (cond ((< num (/ n 2)) (make-every-combo-help list num))
  ;      (else (make-every-combo-help list (- n num)))))

;(define (make-every-combo-help list num)
 ; ((cond ((= num 1) (leave-one-out list))
  ;      (else ))))

(define (every3 fn list)
  (cond ((null? list) ())
        (else (cons (fn (car list)) (every3 fn (cdr list))))))

(define (leave-one-out list)
  (every3 (lambda (x) (remove-from-list x list)) list))

(define (make-every-combo list num)
  (make-every-combo-help list (- (length list) num)))

(define (make-every-combo-help lst num)
  (cond ((= num 0) (list lst))
        (else (accumulate append (every3 (lambda (x) (make-every-combo-help x (- num 1))) (leave-one-out lst))))))

(define (max cmp-fn list)
  (cond ((null? (cdr list)) (car list))
        ((cmp-fn (car (cdr list)) (car list)) (max cmp-fn (cdr list)))
        (else (max cmp-fn (cons (car list) (cdr (cdr list)))))))

(define (poker-value3 hand)
  (let ((suit-name (suit hand)) (r-i (rank-info hand)))
    (cond ((and suit-name  (equal? (straight? hand) '(aces)) (append '(1) '(royal flush --)  suit-name))) 
          ((and suit-name (straight? hand)) (append '(2) '(straight flush of)(list suit-name)))
          ((member? 4 r-i) (append '(3) '(four of a kind --) (list (first (rank-name 4 r-i)))))
          ((and (member? 3 (rank-info hand))
              (member? 2 (rank-info hand))) (append '(4) '(full house --) (list (first (rank-name 3 r-i))) '(over) (list (first (rank-name 2 r-i)))))
          (suit-name (append '(5) '(flush --) suit-name))
          ((straight? hand) (append '(6) '(straight --) (straight? hand) '(high)))
          ((member? 3 (rank-info hand)) (append '(7) '(three of a kind --) (list (first (rank-name 3 r-i)))))
          ((= 2 (count (rank-name 2 r-i))) (append '(8) '(two pair --) (list (first (rank-name 2 r-i)))))
          ((member? 2 (rank-info hand)) (append '(9) '(pair --) (list (first (rank-name 2 r-i)))))
          (else (append '(10) '(nothing))))))

(define (poker-mod-1 hand wild)
  (cdr (max (lambda (a b) (< (car a) (car b))) (every3 (lambda (x) (poker-mod-2 x wild)) (make-every-combo hand 5)))))

(define (poker-mod-2 hand wild)
  (let ((suit-name (suit2 hand wild)) (r-i (rank-info2 hand wild)))
    (cond ((and suit-name  (equal? (straight2? hand wild) '(aces)) (append '(1) '(royal flush --)  suit-name)))
          ((member? 5 r-i)(append '(2) '(five of a kind --) (rank-name 5 r-i)))
          ((and suit-name (straight2? hand wild)) (append '(3) '(straight flush --) suit-name (straight2? hand wild)))
          ((member? 4 r-i) (append '(4) '(four of a kind --) (rank-name 4 r-i)))
          ((and (member? 3 r-i)
              (member? 2 r-i)) (append '(5) '(full house --) (rank-name 3 r-i) '(over) (rank-name 2 r-i)))
          (suit-name (append '(6) '(flush --) suit-name))
          ((straight2? hand wild) (append '(7) '(straight --) (straight2? hand wild) '(high)))
          ((member? 3 r-i) (append '(8) '(three of a kind --) (rank-name 3 r-i)))
          ((= 2 (count (rank-name 2 r-i))) (append '(9) '(two pair --) (rank-name 2 r-i)))
          ((member? 2 r-i) (append '(10) '(pair --) (rank-name 2 r-i)))
          (else (append '(11) '(nothing))))))

(define (suit2 hand wild)
  (cond ((null? (cdr hand)) (trnslte (first (first hand)) *translating-list*))
        ((equal? (first (first hand)) (first (first (butfirst hand))))  (suit2 (butfirst hand) wild))
        ((or (equal? (butfirst (first hand)) wild)
             (equal? (butfirst (first (butfirst hand))) wild)) (suit2 (butfirst hand) wild))
        (else #f)))

(define (rank-info2 cards wild)
  (rank-info2-help (rank-info (first (sort-wild cards wild))) (last (sort-wild cards wild))))

(define (rank-info2-help hand-info num-of-wilds)
  (every3 (lambda (x) (cond ((equal? x (max > hand-info)) (+ x num-of-wilds))
                           (else x))) hand-info))

(define (sort-wild hand wild)
  (append (list (keep (lambda (x) (cond ((equal? (butfirst x) wild) #f)
                          (else #t))) hand)) (list (how-many wild hand))))

(define (how-many rank hand)
  (how-many-help rank (rank-info hand) *list-of-ranks*))

(define (how-many-help rank hand-info ref-list)
  (cond ((null? hand-info) 0)
        ((equal? rank (first ref-list)) (first hand-info))
        (else (how-many-help rank (butfirst hand-info) (butfirst ref-list)))))

(define (vprint x)
  (print (list 'printing: x))
  (newline)
  x)

(define (straight2? hand wild)
  (let ((sw (sort-wild hand wild)))
    (straight2?-helper2 (rank-info (first sw))(last sw) *list-of-ranks*)))
 
(define (straight2?-helper2 sub-hand-info num-of-wilds ref-list)
  (cond ((null? sub-hand-info) #f)
        ((straight2?-helper sub-hand-info num-of-wilds 0 ref-list) (trnslte (first ref-list) *translating-list*))
        (else (straight2?-helper2 (butfirst sub-hand-info) num-of-wilds (butfirst ref-list)))))

(define (straight2?-helper sub-hand-info num-of-wilds num-in-a-row ref-list)
  (cond ((equal? num-in-a-row 5) #t)
        ((empty? sub-hand-info) #f)
        ((< 0 (first sub-hand-info))(straight2?-helper (butfirst sub-hand-info) num-of-wilds (+ num-in-a-row 1) (butfirst ref-list)))
        ((= 0 num-of-wilds) #f)
        (else (straight2?-helper (butfirst sub-hand-info) (- num-of-wilds 1) (+ num-in-a-row 1) (butfirst ref-list)))))

(define h '(d10 dj c3 dk da))
  
;project: scoring bridge hands

(define (card-val card)
  (cond ((number? (butfirst card)) 0)
        ((equal? (butfirst card) 'a) 4)
        ((equal? (butfirst card) 'k) 3)
        ((equal? (butfirst card) 'q) 2)
        ((equal? (butfirst card) 'j) 1)))

(define (high-card-points cards)
  (accumulate + (every card-val cards)))

(define (count-suit suit cards)
  (cond ((empty? cards) 0)
        ((equal? (first (first cards)) suit) (+ 1 (count-suit suit (butfirst cards))))
        (else (count-suit suit (butfirst cards)))))

(define (count-suit2 suit cards)
  (accumulate + (every (lambda (x) (if (equal? (first x) suit) 1 0)) cards)))

(define (suit-counts hand)
  (every (lambda (x) (count-suit x hand)) '(s h c d)))

(define (suit-dist-points num-of-suit)
  (cond ((= num-of-suit 0) 3)
        ((= num-of-suit 1) 2)
        ((= num-of-suit 2) 1)
        (else 0)))

(define (hand-dist-points hand)
  (accumulate + (every suit-dist-points (suit-counts hand))))

(define (bridge-val hand)
  (+ (hand-dist-points hand) (high-card-points hand)))

;Chapter 16

;checks if two sentences are equal
(define (sent-equal? sent1 sent2)
  (cond ((empty? sent1) (empty? sent2))
        ((empty? sent2) #f)
        ((equal? (first sent1) (first sent2)) (sent-equal? (butfirst sent1) (butfirst sent2)))
        (else #f)))

(define (match? pattern sent)
  (cond ((empty? pattern)(empty? sent))
        ((equal? (first pattern) '?)(cond ((empty? sent) (match? (butfirst pattern) sent))
                                          (else (or (match? (butfirst pattern) (butfirst sent))
                                                    (match? (butfirst pattern) sent))))) ;second version: ! and ? (returns #t or #f)
        ;((equal? (first pattern) '*)(*-longest-match (bf pattern) sent)) ;third version: !, ? and * (returns #t or #f)
        ((special? (first pattern))(match-special (first pattern)(butfirst pattern) sent))
        ((empty? sent) #f)
        ((equal? (first pattern) '!) (match? (butfirst pattern) (butfirst sent))) ;first version: ! only (returns #t or #f)
        ((equal? (first pattern) (first sent))(match? (butfirst pattern)(butfirst sent)))
        (else #f)))

;recursive subprogram that deals with (#t or #f) *s
(define (*-longest-match pattern-rest sent)
  (*-lm-helper pattern-rest sent '()))

(define (*-lm-helper pattern-rest sent-matched sent-unmatched)
  (cond ((match? pattern-rest sent-unmatched) #t)
        ((empty? sent-matched) #f)
        (else (*-lm-helper pattern-rest
                           (bl sent-matched)
                           (se (last sent-matched) sent-unmatched)))))

;combined longest match program
(define (longest-match pattern-rest sent min max-one?)
  (cond ((empty? sent) (and (= min 0)
                            (match? pattern-rest sent)))
        (max-one? (lm-helper pattern-rest (sentence (first sent)) (bf sent) min))
        (else (lm-helper pattern-rest sent '() min))))

(define (lm-helper pattern-rest sent-matched sent-unmatched min)
  (cond ((< (length sent-matched) min) #f)
        ((match? pattern-rest sent-unmatched) #t)
        ((empty? sent-matched) #f)
        (else (lm-helper pattern-rest (butlast sent-matched) (sentence (last sent-matched) sent-unmatched) min))))

(define (special? wd)
  (member? wd '(* ? ! &)))

(define (match-special placeholder pattern-rest sent)
  (cond ((equal? placeholder '?)(longest-match pattern-rest sent 0 #t))
        ((equal? placeholder '!)(longest-match pattern-rest sent 1 #t))
        ((equal? placeholder '*)(longest-match pattern-rest sent 0 #f))
        ((equal? placeholder '&)(longest-match pattern-rest sent 1 #f))))