#lang racket

; Jonathan Davis
; CSC 4010 - 800 - Programming Languages
; Scheme/Racket String Search and LL Parser Generator
; Due: November 8th, 2017 @ 11:59pm
; Dr. Martha Kosa



;;
;; Code written in Racket using the DrRacket interpreter.
;; I started on this assignment before we were given the
;; original skeleton file, and in that time frame I made
;; my own convienence functions that are similar to the
;; ones in the skeleton file. I used my own methods to obtain
;; First, Follow, and Predict sets and ultimately generate
;; the parse table. I left all given skeleton functions
;; in so as to make sure parsing performs correctly.
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Begin Skeleton ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LL parser in Scheme.
;; 
;; Written by Michael L. Scott for CSC 254, September 2004
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; This first section of the file contains utility routines
;; that you may find helpful.  I recommend you read all the
;; routines carefully.  Understanding them (really, deeply
;; understanding them) will help you establish the mindset
;; you need to write the rest of the code.
;;
;;#lang racket

(define sort
  (lambda (L)
    ; Use string comparison to quicksort list.
    (letrec ((partition
              (lambda (e L A B)
                (if (null? L) (cons A B)
                    (let ((c (car L)))
                      (if (string<? c e)
                          (partition e (cdr L) (cons c A) B)
                          (partition e (cdr L) A (cons c B))))))))
      (cond
        ((null? L) L)
        ((null? (cdr L)) L)
        (else (let* ((AB (partition (car L) (cdr L) '() '()))
                     (A (car AB))
                     (B (cdr AB)))
                (append (sort A)
                        (list (car L))
                        (sort B))))))))

(define unique
  (lambda (L)
    ; Return list in which adjacent equal elements have been combined.
    (cond
      ((null? L) L)
      ((null? (cdr L)) L)
      ((equal? (car L) (cadr L)) (unique (cdr L)))
      (else (cons (car L) (unique (cdr L)))))))

(define unique-sort
  (lambda (L)
    ; Sort (using string-ified elements) and remove duplicates.
    (unique (sort L))))

(define flatten
  (lambda (L)
    ; Return left-to-right fringe of tree as list.
    (cond
      ((null? L) L)
      ((list? (car L)) (append (flatten (car L)) (flatten (cdr L))))
      (else (cons (car L) (flatten (cdr L)))))))

(define start-symbol
  (lambda (grammar)
    (caar grammar)))

(define last
  (lambda (L)
    ; Return last element of list.
    (cond
      ((null? L) '())
      ((null? (cdr L)) (car L))
      (else (last (cdr L))))))

(define end-marker
  (lambda (grammar)
    (last (cadar grammar))))

(define non-terminals
  (lambda (grammar)
    ; Return list of all non-terminals in grammar.
    (map car grammar)))

(define gsymbols
  (lambda (grammar)
    ; Return list of all symbols in grammar (no duplicates).
    (unique-sort (flatten grammar))))

(define terminals
  (lambda (grammar)
    ; Return list of all terminals in grammar.
    (apply append
           (map (lambda (x) (if (non-terminal? x grammar) '() (list x)))
                (gsymbols grammar)))))

(define productions
  (lambda (grammar)
    ; Return list of all productions in grammar.
    ; Each is represented as a (lhs rhs) pair, where rhs is a list.
    (apply append
           (map (lambda (prods)
                  (map (lambda (rhs)
                         (list (car prods) rhs))
                       (cdr prods)))
                grammar))))

(define non-terminal?
  (lambda (x grammar)
    ; Is x a non-terminal?
    (not (not (member x (non-terminals grammar))))))
    ; 'not not' makes return type a boolean, not a list

(define gsymbol?
  (lambda (x grammar)
    ; is x a symbol in grammar?
    ; (note that name symbol? is taken by Scheme)
    (not (not (member x (gsymbols grammar))))))
    ; 'not not' makes return type a boolean, not a list

(define terminal?
  (lambda (x grammar)
    ; Is x a terminal in grammar?
    (and (member x (gsymbols grammar))
         (not (member x (non-terminals grammar))))))

(define union
  (lambda sets
    (unique-sort (apply append sets))))


;;
;; Much of the following employs a "knowledge" structure.
;; It's a list of 4-tuples, one for each non-terminal,
;;    in the same order those non-terminals appear in the grammar
;;    (the order is important).
;; You are not required to organize things this way, but it's a
;; reasonable approach if you don't have other ideas.
;;
;; The fields of the 4-tuple are:
;;   car     non-terminal A [not needed computationally,
;;           but included for readability of output]
;;   cadr    Boolean: do we currently think A -->* epsilon
;;   caddr   (current guess at) FIRST(A) - {epsilon}
;;   cadddr  (current guess at) FOLLOW(A) - {epsilon}
;;

(define initial-knowledge
  (lambda (grammar)
    ; Return knowledge structure with empty FIRST and FOLLOW sets
    ; and false gen-epsilon estimate for all symbols.
    (map (lambda (A) (list A #f '() '()))
         (non-terminals grammar))))

(define symbol-knowledge
  (lambda (A knowledge)
    ; Return knowledge vector for A.
    (assoc A knowledge)))

(define lookup
  (lambda (nt t parse-tab)
    ; Double-index to find prediction for non-terminal nt and terminal t.
    ; Return #f if not found.
    (letrec ((helper
              (lambda (L)
                (cond
                  ((null? L) #f)
                  ((member t (caar L)) (cadar L))
                  (else (helper (cdr L)))))))
      (helper (cdr (assoc nt parse-tab))))))

(define display-list
  (lambda (L)
    ; Print list to standard output.
    ; Yes, this is imperative.
    (if (not (null? L))
        (begin (display (string-append " " (car L))) (display-list (cdr L)))
        (display '()) ;; added by MJK
                 )))

(define parse
  (lambda (grammar input)
    ; Parse input according to grammar.
    ; Print predictions and matches (imperatively) along the way.
    ; You can also print the stack and remaining input at each step,
    ; if you want: simply uncomment the ';;;' line below
    ; Return #t if the input is in the language; #f if it's not.
    (letrec
        ((die (lambda (s) (begin (display "syntax error: ") (display s) (newline) #f)))
         (parse-tab (parse-table grammar))
         (helper
          (lambda (stack input)
            (begin
;;;           (display stack) (display input) (newline)
              (cond
                ((null? stack)
                 (or (null? input) (die "extra input beyond end of program")))
                ((terminal? (car stack) grammar)
                 (if (equal? (car stack) (car input))
                     (begin
                       (display (string-append "   match " (car input)))
                       (newline)
                       (helper (cdr stack) (cdr input)))
                     (die (string-append "expected " (car stack) "; saw " (car input)))))
                (else ; non-terminal
                 (let ((rhs (lookup (car stack) (car input) parse-tab)))
                   (if rhs
                       (begin
                         (display (string-append "   predict " (car stack) " ->"))
                         (display-list rhs)
                         (newline)
                         (helper (append rhs (cdr stack)) input))
                       (die (string-append "no prediction for " (car stack)
                                           " when seeing " (car input)))))))))))
      (helper (list (start-symbol grammar)) input))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; End Skeleton ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Determines if every element in list1 is in the same order at
; the front of list2. If so, then list1 is a sublist of list2.
(define contiguous?
	(lambda (list1 list2)
		(if (null? list1) #t
			(if (null? list2) #f
				(if (equal? (car list1) (car list2))
					(contiguous? (cdr list1) (cdr list2))
				;else
					#f
				)
			)
		)
	)
)

; Determines if the first element of list1 is even in list2.
; If there is an occurence, it calls contiguous? to see if the
; following elements in list2 are the same elements in the same order
; as the rest of list1. If so, then list1 is a sublist of list2.
(define sublist?
	(lambda (list1 list2)
		(if (null? list1) #t
			(if (null? list2) #f 
				(if (equal? (car list1) (car list2)) 
					(if (contiguous? (cdr list1) (cdr list2)) #t
						(sublist? list1 (cdr list2))
					)
				;else
					(sublist? list1 (cdr list2))
				)
			)
		)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;Determines whether the contents of list1 are a sublist of the head of the list in list2.
;If they are, then the return list, list3, is appended with the contents of list2. 
;This process repeats until all lists within list2 have been evaluated.
(define lgrep_rec
	(lambda (list1 list2 list3)
		(if (null? list2) 
			(reverse list3)
		;else
			(if (sublist? list1 (car list2))
				(lgrep_rec list1 (cdr list2) (append (cons (car list2) '()) list3))
			;else
				(lgrep_rec list1 (cdr list2) list3)
			)
		)
	)
)

;Returns a list containing any lists from list2 in which the contents of list1 are
;a sublist of.
(define lgrep
	(lambda (list1 list2)
		(lgrep_rec list1 list2 '())
	)
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define nonterminal-list_rec
	(lambda (grammar list1)
		(if (null? grammar)
			(reverse list1)
		;else
			(nonterminal-list_rec (cdr grammar) (append (cons (caar grammar) '()) list1))
		)
	)
)

;Given a grammar, this function will return a list of all unique nonterminal symbols
;appearing on the LHS of all rules in the grammar.
(define nonterminal-list
	(lambda (grammar)
		(nonterminal-list_rec grammar '())
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define get-RHS-rules_rec
	(lambda (RHS list1)
		(if (null? RHS)
			(reverse list1)
		;else
			(get-RHS-rules_rec (cdr RHS) (append (cons (car RHS) '()) list1))
		)
	)
)

(define get-RHS-rules
	(lambda (RHS)
		(get-RHS-rules_rec RHS '())
	)
)

;Given a grammar and a nonterminal symbol from the LHS of a rule, this function will
;return a list of all values on the RHS corresponding to that nonterminal in the grammar.
(define get-productions
	(lambda (grammar LHS)
		(if (equal? (caar grammar) LHS)
			(get-RHS-rules (cdar grammar))
		;else
			(get-productions (cdr grammar) LHS)
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define generates-epsilon_rec
	(lambda (grammar productions)
		(if (null? productions) #f
			(if (equal? (car productions) '())
				#t
			;else
				(or 
					(generates-epsilon_rec grammar (cdr productions)) 
					(if (memq (caar productions) (nonterminal-list grammar))
						(and 
							(generates-epsilon? grammar (caar productions))
							(generates-epsilon_rec grammar (cons (cdar productions) '()))
						)
					;else
						#f
					)
				)
			)
		)
	)
)

(define generates-epsilon?
	(lambda (grammar nonterminal)
		(if (memq nonterminal (nonterminal-list grammar))
			(generates-epsilon_rec grammar (get-productions grammar nonterminal))
		;else
			#f
		)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define shortener
	(lambda (list1)
		(if (pair? list1)
			(if (pair? (car list1))
				(shortener (car list1))
			;else
				(reverse list1)
			)
		;else
			(cons (reverse list1) '())
		)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;tales in something like ("S" "SL") for the productions paratmeter
(define prod_eval
	(lambda (grammar productions list1)
		(if (null? productions)
			(reverse list1)
		;else
			(if (memq (car productions) (nonterminal-list grammar))
				(if (generates-epsilon? grammar (car productions))
					(append(append (first-set grammar (car productions)) (shortener(cons (prod_eval grammar (cdr productions) '()) '()))) '("EPSILON"))
				;else
					(first-set grammar (car productions))
				)
			;else
				(car productions)
			)
		)
	)
)



;takes in something like (("S" "SL") ()) for the productions parameter
(define productions_eval
	(lambda (grammar productions list1)
		(if (null? productions)
			(reverse list1)
		;else
			(productions_eval grammar (cdr productions) (append (shortener (cons (prod_eval grammar (car productions) '()) '())) (shortener (cons list1 '()))))
		)
	)
)



(define epsilon-check
	(lambda (list1)
		(if (memq "EPSILON" list1)
			(append (remove* '("EPSILON") list1) '(()))
		;else
			list1
		)
	)
)



(define first-set
	(lambda (grammar symbol)
		(if (equal? symbol '())
			'()
		;else
			(if (memq symbol (nonterminal-list grammar))
				(if (generates-epsilon? grammar symbol)
					(append (remove* '(()) (productions_eval grammar (get-productions grammar symbol) '())) '(()))
				;else
					(epsilon-check (remove* '(()) (productions_eval grammar (get-productions grammar symbol) '())))
				)
			;else
				(cons symbol '()) ;its actually a terminal symbol
			)
		)
	)
)



(define first-set_production
	(lambda (grammar production list1)
		(if (null? production)
			(if (null? list1)
				'(())
			;else
				list1
			)
		;else
			(if (equal? (car production) '())
				(append '(()) list1)
			;else
				(if (memq (car production) (nonterminal-list grammar))
					(if (generates-epsilon? grammar (car production))
						(first-set_production grammar (cdr production) (append (remove* '(()) (productions_eval grammar (get-productions grammar (car production)) '())) '(())))
					;else
						(append (epsilon-check (remove* '(()) (productions_eval grammar (get-productions grammar (car production)) '()))) list1)
					)
				;else
					(append (cons (car production) '()) list1) ;its actually a terminal symbol
				)
			)
		)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


		
(define get-after
	(lambda (symbol list1)
		(if (null? list1)
			#f
		;else
			(if (equal? symbol (car list1))
				(if (null? (cdr list1))
					'(())
				;else
					(cdr list1)
				)
			;else
				(get-after symbol (cdr list1))
			)	
		)
	)
)		


	
(define analyze-productions
	(lambda (LHS symbol list1 list2)
		(if (null? list1)
			list2
		;else
			(if (memq symbol (car list1))
				(analyze-productions LHS symbol (cdr list1) (append (cons (cons LHS (cons (get-after symbol (car list1)) '())) '()) list2))
			;else
				(analyze-productions LHS symbol (cdr list1) list2)
			)
		)
	)
)		



(define right-context
	(lambda (grammar symbol list1)
		(if (null? grammar)
			(reverse list1)
		;else
			(if (null? (analyze-productions (caar grammar) symbol (cdar grammar) '()))
				(right-context (cdr grammar) symbol list1)
			;else
				(right-context (cdr grammar) symbol (append (analyze-productions (caar grammar) symbol (cdar grammar) '()) list1))
			)
		)
	)
)
		

		
;list1 is something like ("P" ("$$")) or ("SL" (()))	
(define process-rule
	(lambda (grammar symbol list1 list2)
		(if (null? (cadr list1))
			(reverse list2)
		;else
			(if (null? (caadr list1))
				(if (equal? symbol (car list1))
					'()
				;else
					(follow-set_call grammar (car list1) list2)
				)
			;else
				(if (generates-epsilon? grammar (caadr list1))
					(if (null? (cdadr list1))
						(process-rule grammar symbol (cons (car list1) '((()))) (append (first-set grammar (caadr list1)) list2))
					;else
						(process-rule grammar symbol (cons (car list1) (cons (cdadr list1) '())) (append (first-set grammar (caadr list1)) list2))
					)
				;else
					(append (first-set grammar (caadr list1) ) list2)
				)
			)
		)
	)
)


		
(define process-right
	(lambda (grammar symbol list1 list2)
		(if (null? list1)
			(reverse list2)
		;else
			(process-right grammar symbol (cdr list1) (append (process-rule grammar symbol (car list1) '()) list2))
		)
	)
)		


		
(define follow-set_call
	(lambda (grammar symbol list1)
		(remove-duplicates (remove* '(()) (process-right grammar symbol (right-context grammar symbol '()) list1)))
	)
)


		
(define follow-set
	(lambda (grammar symbol)
		(if (null? (follow-set_call grammar symbol '()))
			'()			;; Using this will produce a follow set as shown in the example in the assignment instructions
			;'("$")		;; Using this will technically produce the correct follow set, as "$" always is in FOLLOW(Start_Symbol)
		;else
			(follow-set_call grammar symbol '())	
		)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define get-knowledge_rec
	(lambda (grammar list1)
		(if (null? grammar)
			list1
		;else
			(get-knowledge_rec (cdr grammar) (append (cons (list (caar grammar) (generates-epsilon? grammar (caar grammar)) (first-set grammar (caar grammar)) (follow-set grammar (caar grammar))) '()) list1))
		)
	)
)



(define get-knowledge
	(lambda (grammar)
		(reverse (get-knowledge_rec grammar '()))
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;Given a list representing a LHS and its productions such as ("SL" ("S" "SL") ()),
;Calculate the predict sets using knowledge of the grammar.
(define predict
	(lambda (grammar knowledge rules list1)
		(if (null? (cdr rules))
			(append (cons (car rules) '()) (remove* '(()) (reverse list1)))
		;else
			(if (memq '() (first-set_production grammar (cadr rules) '()))			  
				(predict grammar knowledge (append (cons (car rules) '())(cddr rules)) (append (cons (cons (remove* '(()) (append (first-set_production grammar (cadr rules) '()) (follow-set grammar (car rules)))) (cons (cadr rules) '())) '()) list1));(append (cons (remove-duplicates (append (follow-set grammar (car rules)) (first-set_production grammar (cadr rules) '()))) (cons (cadr rules) '())) list1))
			;else
				(predict grammar knowledge (append (cons (car rules) '())(cddr rules)) (append (cons (cons (first-set_production grammar (cadr rules) '()) (cons (cadr rules) '())) '()) list1))
			)
		)
	)
)


(define p-table_rec
	(lambda (grammar knowledge remaining list1)
		(if  (null? remaining)
			(reverse list1)
		;else
			(if (equal? (caar grammar) (caar remaining))
				(p-table_rec grammar knowledge (cdr remaining) (append (cons (predict grammar knowledge (car remaining) '()) '()) list1))
			;else
				(p-table_rec grammar knowledge (cdr remaining) (append (cons (predict grammar knowledge (car remaining) '()) '()) list1))
			)
		)
	)
)

(define parse-table
	(lambda (grammar)
		(let ((knowledge (get-knowledge grammar)))
			(p-table_rec grammar knowledge grammar '())
		)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Part 1 Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;Test to see if sublist? works
(print "sublist? tests:") (newline)
(print(sublist? '(c d e) '(a b c d e f g))) (newline)
(print(sublist? '(a c e) '(a b c d e f g))) (newline)
(print(sublist? '(f) '(a b c d e f g))) (newline)

(newline)

;Test to see if lgrep works
(define longlist
'((a b c d e f g)
  (c d c d e)
  (a b c d)
  (h i c d e k)
  (x y z))
)

(print "lgrep test:") (newline)
(print (lgrep '(c d e) longlist)) (newline)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Part 2 Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;Various tests for LL Parse table generation
(define calc-gram
  '(("P"  ("SL" "$$"))
    ("SL" ("S" "SL") ())
    ("S"  ("id" ":=" "E") ("read" "id") ("write" "E"))
    ("E"  ("T" "TT"))
    ("T"  ("F" "FT"))
    ("TT" ("ao" "T" "TT") ())
    ("FT" ("mo" "F" "FT") ())
    ("ao" ("+") ("-"))
    ("mo" ("*") ("/"))
    ("F"  ("id") ("num") ("(" "E" ")"))
   )
)

(newline)

;First sets for every nonterminal
(print "First sets for calc-gram:") (newline)
(print(first-set calc-gram "P")) (newline)
(print(first-set calc-gram "SL")) (newline)
(print(first-set calc-gram "S")) (newline)
(print(first-set calc-gram "TT")) (newline)
(print(first-set calc-gram "FT")) (newline)
(print(first-set calc-gram "F")) (newline)
(print(first-set calc-gram "ao")) (newline)
(print(first-set calc-gram "mo")) (newline)
(print(first-set calc-gram "T")) (newline)
(print(first-set calc-gram "E")) (newline)

(newline)

;Follow sets for every nonterminal
(print "Follow sets for calc-gram:") (newline)
(print(follow-set calc-gram "P")) (newline)
(print(follow-set calc-gram "SL")) (newline)
(print(follow-set calc-gram "S")) (newline)
(print(follow-set calc-gram "E")) (newline)
(print(follow-set calc-gram "TT")) (newline)
(print(follow-set calc-gram "T")) (newline)
(print(follow-set calc-gram "FT")) (newline)
(print(follow-set calc-gram "F")) (newline)
(print(follow-set calc-gram "ao")) (newline)
(print(follow-set calc-gram "mo")) (newline)

(newline)

;The knowledge structure as specified in the assignment instructions
;Print produces ugly formatting, run the command on DrRacket interpreter for pretty results
(print "Knowledge for calc-gram:") (newline)
(print (get-knowledge calc-gram)) (newline)

(newline)

;The parse table as specified in the assignment instructions
;Print produces ugly formatting, run the command on DrRacket interpreter for pretty results
(print "Parse table for calc-gram:") (newline)
(print (parse-table calc-gram)) (newline)

(newline)

;Testing parses of valid input
(print "Testing valid input: $$") (newline)
(print (parse calc-gram '("$$"))) (newline) (newline)

(print "Testing valid input: read id $$") (newline)
(print (parse calc-gram '("read" "id" "$$"))) (newline) (newline)

(print "Testing valid input: id := num write id - num $$") (newline)
(print (parse calc-gram '("id" ":=" "num" "write" "id" "-" "num" "$$"))) (newline) (newline)

(print "Testing valid input: read id write ( id * num ) $$") (newline)
(print (parse calc-gram '("read" "id" "write" "(" "id" "*" "num" ")" "$$"))) (newline) (newline)

(newline)

;Testing parses of invalid input
(print "Testing invalid input: num $$") (newline)
(print (parse calc-gram '("num" "$$"))) (newline) (newline)

(print "Testing invalid input: id := num $$ num") (newline)
(print (parse calc-gram '("id" ":=" "num" "$$" "num"))) (newline) (newline)

(print "Testing invalid input: write id read id / id $$") (newline)
(print (parse calc-gram '("write" "id" "read" "id" "/" "id" "$$"))) (newline) (newline)

