#lang racket
(require racket/trace)
(require trace)

;---------------------------------------------------------
; Raagam Stuff

;--- Scale Objects ----

(define (scale-object? possible-scale-object)
  (and (list? possible-scale-object)
       (= (length possible-scale-object) 12)
       (only-zeros-and-ones? possible-scale-object)))

(define (make-random-scale-object)
  (build-list 12 (lambda (x) (random 2))))

;returns another scale object

(define (shift-down-scale-object scale-object)
  (cond
    ((not (scale-object? scale-object)) (error "not scale object"))
    (else (append (cdr scale-object) (list (car scale-object))))))

(define (print-scale-object scale-object)
  (cond
    ((not (scale-object? scale-object)) (error "not scale object"))
    (else (print-scale-object1 scale-object 1))))

(define (print-scale-object1 scale-object currentindex)
  (cond
    ((null? scale-object) '())
    ((not (= (car scale-object) 1))(print-scale-object1 (cdr scale-object) (+ currentindex 1)))
    ((= currentindex 1) (cons 'S (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    ((= currentindex 2) (cons 'R1 (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    ((= currentindex 3) (cons 'R2-G1 (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    ((= currentindex 4) (cons 'R3-G2 (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    ((= currentindex 5) (cons 'G3 (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    ((= currentindex 6) (cons 'M1 (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    ((= currentindex 7) (cons 'M2 (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    ((= currentindex 8) (cons 'P (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    ((= currentindex 9) (cons 'D1 (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    ((= currentindex 10) (cons 'D2-N1 (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    ((= currentindex 11) (cons 'D3-N2 (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    ((= currentindex 12) (cons 'N3 (print-scale-object1 (cdr scale-object) (+ currentindex 1))))
    (else (error "something went wrong"))))

(define (sa-exists? scale-object)
  (nth-note-exists? 1 scale-object))

(define (pa-exists? scale-object)
  (nth-note-exists? 8 scale-object))

(define (exactly-one-ma? scale-object)
  (let ((ma1-exists? (nth-note-exists? 6 scale-object))
        (ma2-exists? (nth-note-exists? 7 scale-object)))
    (or (and ma1-exists? (not ma2-exists?))
        (and ma2-exists? (not ma1-exists?)))))

(define (one-ri-one-ga? scale-object)
  (= 2 (apply + (subscale-range scale-object 2 4))))

(define (one-da-one-ni? scale-object)
  (= 2 (apply + (subscale-range scale-object 9 4))))

(define (subscale-range scale-object start-index len)
  (return-first-n (drop-first-n scale-object (- start-index 1)) len))

(define (nth-note-exists? n scale-object)
  (= 1 (list-ref scale-object (- n 1))))

;------------ Scale object-underlying list functions

(define (only-zeros-and-ones? lst)
  (andmap (lambda (item) (or (= item 0) (= item 1))) lst))

(define (drop-first-n lst n)
  (cond
    ((= n 0) lst)
    ((null? lst) (error "list not long enough"))
    (else (drop-first-n (cdr lst) (- n 1)))))

(define (return-first-n lst n)
  (cond
    ((= n 0) '())
    ((null? lst)(error "list not long enough"))
    (else (cons (car lst) (return-first-n (cdr lst) (- n 1))))))


 ;---- Melakarta stuff

;---------Test Cases

(define *kalyani* '(1 0 1 0 1 0 1 1 0 1 0 1))

(define *mohanam* '(1 0 0 1 1 0 0 1 0 1 0 0))

(define *kalyani-plus-another-ri* '(1 0 1 1 1 0 1 1 0 1 0 1))

; takes ragam returns boolean whether is valid melakarta or not
(define (mragam? scale-object)
  (and
   (scale-object? scale-object)
   (sa-exists? scale-object)
   (pa-exists? scale-object)
   (exactly-one-ma? scale-object)
   (one-ri-one-ga? scale-object)
   (one-da-one-ni? scale-object)))


;fn makemragam
; makes melakarta ragam
;takes number returns ragam
(define (make-mragam-num n)
  (cond
    ((not (and (integer? n) (<= n 72) (>= n 1))) (error "invalid ragam number"))
    (else (append (sa-ri-ga-fragment n)(ma-fragment n)(pa-da-ni-fragment n)))))

(define (sa-ri-ga-fragment n)
  (lookup-subscale (quotient (remainder (- n 1) 36) 6)))

(define (pa-da-ni-fragment n)
  (lookup-subscale (remainder (- n 1) 6)))

(define (ma-fragment n)
  (if (> n 36) '(0 1) '(1 0)))

(define (lookup-subscale n)
  (cond
    ((= n 0) '(1 1 1 0 0))
    ((= n 1) '(1 1 0 1 0))
    ((= n 2) '(1 1 0 0 1))
    ((= n 3) '(1 0 1 1 0))
    ((= n 4) '(1 0 1 0 1))
    ((= n 5) '(1 0 0 1 1))
    (else (error "invalid n input"))))

;(trace lookup-subscale)

;mraganum
;takes ragam (melakarta or not)
;returns ragam number or #f
(define (mragam-num scale-object)
  (cond
    ((not (mragam? scale-object)) #f)
    (else (+ (ma-contribution scale-object)
             (ri-ga-contribution scale-object)
             (da-ni-contribution scale-object)))))

(define (ma-contribution scale-object)
  (cond
    ((nth-note-exists? 6 scale-object) 0)
    (else 36)))

(define (ri-ga-contribution scale-object)
  (* 6 (ri-ga-da-ni-contribution-calc scale-object 2)))

(define (da-ni-contribution scale-object)
  (+ 1 (ri-ga-da-ni-contribution-calc scale-object 9)))

(define (ri-ga-da-ni-contribution-calc scale-object startindex)
  (let ((subscale (subscale-range scale-object startindex 4)))
    (cond
      ((equal? subscale '(1 1 0 0)) 0)
      ((equal? subscale '(1 0 1 0)) 1)
      ((equal? subscale '(1 0 0 1)) 2)
      ((equal? subscale '(0 1 1 0)) 3)
      ((equal? subscale '(0 1 0 1)) 4)
      ((equal? subscale '(0 0 1 1)) 5)
      (else (error "something went wrong")))))

(define (test-all-ragams)
  (test-all-ragams-1 72))

(define (test-all-ragams-1 n)
  (cond
    ((= n 0) #t)
    ((not (= n (mragam-num (make-mragam-num n))))(error n))
    (else (test-all-ragams-1 (- n 1)))))

;printragam
;returns readable ragam notation

(define x 12)

(define (melakarta-gruha-bedams n)
  (melakarta-gruha-bedams1 (make-mragam-num n) 0))

(define (melakarta-gruha-bedams1 scale-object currentshifts)
  (cond
    ((= currentshifts 12) '())
    ((mragam? scale-object)(cons (cons (mragam-num scale-object) currentshifts)
                                 (melakarta-gruha-bedams1 (shift-down-scale-object scale-object) (+ currentshifts 1))))
    (else (melakarta-gruha-bedams1 (shift-down-scale-object scale-object) (+ currentshifts 1)))))

(define (all-gruha-bedams)
  (build-list 72 (lambda (n) (melakarta-gruha-bedams (+ n 1)))))

(define (unique-gruha-bedams)
  (remove-duplicates (all-gruha-bedams) same-gruha-bedam?))


(define (same-gruha-bedam? bedam1 bedam2)
  (not (eq? #f (member (car bedam1) bedam2 (lambda (n1.s1 n2.s2)(= (car n1.s1)(car n2.s2)))))))

(define (mragam-name-lookup n)
  (cadr (assoc n *raga-db*)))

(define (unique-gruha-bedams-with-names)
  (map add-ragam-names (unique-gruha-bedams)))

(define (add-ragam-names bedam)
  (map (lambda (n.s) (list (car n.s) (mragam-name-lookup (car n.s)) (cdr n.s))) bedam))

;final list - all unique gruha bedam cycles works with wikipedia graha bedam article
(define (unique-gruha-bedam-cycles)
  (filter (lambda (bedam) (> (length bedam) 1)) (unique-gruha-bedams-with-names)))

;---------------------- RAGA DB
(define *raga-db*  
  '(
    (01     kanakAngi       (S R1 G1 M1 P D1 N1 S) (S N1 D1 P M1 G1 R1 S))
    (02     rathnAngi       (S R1 G1 M1 P D1 N2 S)  (S N2 D1 P M1 G1 R1 S))
    (03     gAnamUrthi      (S R1 G1 M1 P D1 N3 S)  (S N3 D1 P M1 G1 R1 S))
    (04     vanaspathi      (S R1 G1 M1 P D2 N2 S)  (S N2 D2 P M1 G1 R1 S))
    (05     mAnavathi       (S R1 G1 M1 P D2 N3 S)  (S N3 D2 P M1 G1 R1 S))
    (06     thAnarUpi       (S R1 G1 M1 P D3 N3 S)  (S N3 D3 P M1 G1 R1 S))
    (07     sEnAvathi       (S R1 G2 M1 P D1 N1 S)  (S N1 D1 P M1 G2 R1 S))
    (08     HanumathOdi     (S R1 G2 M1 P D1 N2 S)  (S N2 D1 P M1 G2 R1 S))
    (09     DhEnukA (S R1 G2 M1 P D1 N3 S)  (S N3 D1 P M1 G2 R1 S))
    (10     nAtakapriya     (S R1 G2 M1 P D2 N2 S)  (S N2 D2 P M1 G2 R1 S))
    (11     kOkilapriya     (S R1 G2 M1 P D2 N3 S)  (S N3 D2 P M1 G2 R1 S))
    (12     rUpavathi       (S R1 G2 M1 P D3 N3 S)  (S N3 D3 P M1 G2 R1 S))
    (13     gAyakapriya     (S R1 G3 M1 P D1 N1 S)  (S N1 D1 P M1 G3 R1 S))
    (14     vakulAbharaNam  (S R1 G3 M1 P D1 N2 S)  (S N2 D1 P M1 G3 R1 S))
    (15     mAyAmALavagowLA (S R1 G3 M1 P D1 N3 S)  (S N3 D1 P M1 G3 R1 S))
    (16     chakravAkam     (S R1 G3 M1 P D2 N2 S)  (S N2 D2 P M1 G3 R1 S))
    (17     sUryakAntam     (S R1 G3 M1 P D2 N3 S)  (S N3 D2 P M1 G3 R1 S))
    (18     HAtakAmbari     (S R1 G3 M1 P D3 N3 S)  (S N3 D3 P M1 G3 R1 S))
    (19     JankAradhvani   (S R2 G2 M1 P D1 N1 S)  (S N1 D1 P M1 G2 R2 S))
    (20     naTabhairavi    (S R2 G2 M1 P D1 N2 S)  (S N2 D1 P M1 G2 R2 S))
    (21     kIravANi        (S R2 G2 M1 P D1 N3 S)  (S N3 D1 P M1 G2 R2 S))
    (22     KaraHarapriya   (S R2 G2 M1 P D2 N2 S)  (S N2 D2 P M1 G2 R2 S))
    (23     gowrimanOHari   (S R2 G2 M1 P D2 N3 S)  (S N3 D2 P M1 G2 R2 S))
    (24     varuNapriya     (S R2 G2 M1 P D3 N3 S)  (S N3 D3 P M1 G2 R2 S))
    (25     mAraranjani     (S R2 G3 M1 P D1 N1 S)  (S N1 D1 P M1 G3 R2 S))
    (26     chArukeshi      (S R2 G3 M1 P D1 N2 S)  (S N2 D1 P M1 G3 R2 S))
    (27     sarasAngi       (S R2 G3 M1 P D1 N3 S)  (S N3 D1 P M1 G3 R2 S))
    (28     HarikAmbhOji    (S R2 G3 M1 P D2 N2 S)  (S N2 D2 P M1 G3 R2 S))
    (29     DhIrashankarAbharaNam   (S R2 G3 M1 P D2 N3 S)  (S N3 D2 P M1 G3 R2 S))
    (30     nAgAnandhini    (S R2 G3 M1 P D3 N3 S)  (S N3 D3 P M1 G3 R2 S))
    (31     yAgapriya       (S R3 G3 M1 P D1 N1 S)  (S N1 D1 P M1 G3 R3 S))
    (32     rAgavarDhani    (S R3 G3 M1 P D1 N2 S)  (S N2 D1 P M1 G3 R3 S))
    (33     gAngeyabhushani (S R3 G3 M1 P D1 N3 S)  (S N3 D1 P M1 G3 R3 S))
    (34     vAgaDhIsvari    (S R3 G3 M1 P D2 N2 S)  (S N2 D2 P M1 G3 R3 S))
    (35     shUlini (S R3 G3 M1 P D2 N3 S)  (S N3 D2 P M1 G3 R3 S))
    (36     chalanAta       (S R3 G3 M1 P D3 N3 S)  (S N3 D3 P M1 G3 R3 S))
    (37     sAlagam (S R1 G1 M2 P D1 N1 S)  (S N1 D1 P M2 G1 R1 S))
    (38     jalArnavam      (S R1 G1 M2 P D1 N2 S)  (S N2 D1 P M2 G1 R1 S))
    (39     JAlavarALi      (S R1 G1 M2 P D1 N3 S)  (S N3 D1 P M2 G1 R1 S))
    (40     navanItham      (S R1 G1 M2 P D2 N2 S)  (S N2 D2 P M2 G1 R1 S))
    (41     pAvani  (S R1 G1 M2 P D2 N3 S)  (S N3 D2 P M2 G1 R1 S))
    (42     raGupriya       (S R1 G1 M2 P D3 N3 S)  (S N3 D3 P M2 G1 R1 S))
    (43     gavAmbodhi      (S R1 G2 M2 P D1 N1 S)  (S N1 D1 P M2 G2 R1 S))
    (44     bhavapriya      (S R1 G2 M2 P D1 N2 S)  (S N2 D1 P M2 G2 R1 S))
    (45     shubhapanthuvarALi      (S R1 G2 M2 P D1 N3 S)  (S N3 D1 P M2 G2 R1 S))
    (46     shadhvidhamArgiNi       (S R1 G2 M2 P D2 N2 S)  (S N2 D2 P M2 G2 R1 S))
    (47     suvarNAngi      (S R1 G2 M2 P D2 N3 S)  (S N3 D2 P M2 G2 R1 S))
    (48     dhivyAmaNi      (S R1 G2 M2 P D3 N3 S)  (S N3 D3 P M2 G2 R1 S))
    (49     dhavalAmbari    (S R1 G3 M2 P D1 N1 S)  (S N1 D1 P M2 G3 R1 S))
    (50     nAmanArAyaNi    (S R1 G3 M2 P D1 N2 S)  (S N2 D1 P M2 G3 R1 S))
    (51     kAmavardhini-Pantuvarali        (S R1 G3 M2 P D1 N3 S)  (S N3 D1 P M2 G3 R1 S))
    (52     rAmapriya       (S R1 G3 M2 P D2 N2 S)  (S N2 D2 P M2 G3 R1 S))
    (53     gamanashrama    (S R1 G3 M2 P D2 N3 S)  (S N3 D2 P M2 G3 R1 S))
    (54     vishvAmbhari    (S R1 G3 M2 P D3 N3 S)  (S N3 D3 P M2 G3 R1 S))
    (55     shyAmaLAngi     (S R2 G2 M2 P D1 N1 S)  (S N1 D1 P M2 G2 R2 S))
    (56     shanmuKapriya   (S R2 G2 M2 P D1 N2 S)  (S N2 D1 P M2 G2 R2 S))
    (57     simHendramadhyamam      (S R2 G2 M2 P D1 N3 S)  (S N3 D1 P M2 G2 R2 S))
    (58     HemAvathi       (S R2 G2 M2 P D2 N2 S)  (S N2 D2 P M2 G2 R2 S))
    (59     DharmAvathi     (S R2 G2 M2 P D2 N3 S)  (S N3 D2 P M2 G2 R2 S))
    (60     nIthimathi      (S R2 G2 M2 P D3 N3 S)  (S N3 D3 P M2 G2 R2 S))
    (61     kAnthAmaNi      (S R2 G3 M2 P D1 N1 S)  (S N1 D1 P M2 G3 R2 S))
    (62     rishabhapriya   (S R2 G3 M2 P D1 N2 S)  (S N2 D1 P M2 G3 R2 S))
    (63     lathAngi        (S R2 G3 M2 P D1 N3 S)  (S N3 D1 P M2 G3 R2 S))
    (64     vAchaspathi     (S R2 G3 M2 P D2 N2 S)  (S N2 D2 P M2 G3 R2 S))
    (65     mEchakalyANi    (S R2 G3 M2 P D2 N3 S)  (S N3 D2 P M2 G3 R2 S))
    (66     chithrAmbari    (S R2 G3 M2 P D3 N3 S)  (S N3 D3 P M2 G3 R2 S))
    (67     sucharithra     (S R3 G3 M2 P D1 N1 S)  (S N1 D1 P M2 G3 R3 S))
    (68     jyothisvarUpiNi (S R3 G3 M2 P D1 N2 S)  (S N2 D1 P M2 G3 R3 S))
    (69     dhAtuvardhani   (S R3 G3 M2 P D1 N3 S)  (S N3 D1 P M2 G3 R3 S))
    (70     nAsikabhUshaNi  (S R3 G3 M2 P D2 N2 S)  (S N2 D2 P M2 G3 R3 S))
    (71     kosalam (S R3 G3 M2 P D2 N3 S)  (S N3 D2 P M2 G3 R3 S))
    (72     rasikapriya     (S R3 G3 M2 P D3 N3 S)  (S N3 D3 P M2 G3 R3 S))
    ))
