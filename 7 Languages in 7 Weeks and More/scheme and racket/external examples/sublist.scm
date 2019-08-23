; Jonathan Davis
; CSC 4010 - 800 - Programming Languages
; Scheme/Racket Strign Search and LL Parser Generator
; Due: November 8th, 2017 @ 11:59pm
; Dr. Martha Kosa



;;
;; Code written in Racket using the DrRacket interpreter.
;; Output of the parse-table function was copied and pasted
;; into a chicken scheme environment on ideone and tested
;; using the provided parse function from the skeleton file.
;; (Skeleton file functions do not follow Racket syntax rules)
;;



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