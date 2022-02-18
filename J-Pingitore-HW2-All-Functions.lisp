;Author: Jesse Pingitore
;---------------------- separates questions.

;Non-trivial Built-ins used: 
;(length) to determine the length of lists
;(member) to test membership of elements
;(numberp) to test if an atom is a number or a symbol


; Is this a set? (Unique elements only)
; param s: List to determine set-iness
; return: Bool, is this a set
(defun setp (s)
    (if (and s (listp s))
        (if (= 1 (length s))
            T ;it's a set!

            ;else, there is 2+ elements, need to test
            
            (if (member (car s) (cdr s) :test 'equal)
                ;if the first el is a member in the rest

                nil ;false, will return false

                ;else, recurse with the cdr
                (setp (cdr s))
            )
        )
        ;else, s is nil. we need to catch it or it explodes
        nil
    )
)

; Function to insert an element into a setp
; param ele: the element to insert
; param s: the set. may be given as atom or nil,
;   it will be converted into a list 
; return: a (copied) version of s with ele included
(defun insert (ele s)

    ;make sure s isn't an atom
    (if (listp s)

        ;if ele is not nil, check if member
        (if ele

            ;if the element is already in s
            (if (member ele s :test 'equal)

                ;return s plainly
                s

                ;else, add it to the front
                (cons ele s);return a concat'd version of s
            )

            ;else ele is nil, return s
            s
        )

        ;else s is atom (not nil), convert to lst
        (insert ele (list s));and do this again
    )
)

; Make a list into a set.
; Demonstrates how important the stack is in lisp.
; param bag: the list to convert
; return: the bag minus any duplicates, last appearence
;   of a duplicated value is retained
(defun makeset (bag)
    (if (and bag (listp bag));ensure bag is not nil and a list
        (if (= 1 (length bag))
            bag ;base case

            ;else, there is 2+ elements, need to test
            (if (member (car bag) (cdr bag) :test 'equal)
                
                ;duplicate!
                (makeset (cdr bag)) ;this will only return the cdr
                                ;minus the dup'd ele

                ;no duplicate, retain ele in the stack and check the rest
                (cons (car bag) (makeset (cdr bag)))
            )
        )

        ;else catch a nil bag or atom bag to avoid crashing swank
        (if bag
            (list bag);atom, wrap in a list
            nil ;TODO VERIFY THIS IS RIGHT
        )
    )
)

; Return the relative complement of the s1, s1^s2.
; NOTE: Sets may contains lists, but not other sets.
; (a (b ,c)) != (a, (c b))
; param s1: The set to subtract from
; param s2: The set to compare against
; return: relative complement of s1, s1^s2.
(defun rel-complement (s1 s2)
    (if (null s2);if s2 is nil, return s1
        s1
        (if (null s1);if s1 is nil, return nil
            nil
            ;else check length
            (if (= 1 (length s1)) ;base case
                (if (member (car s1) s2 :test 'equal)
                    nil
                    s1
                )
                ;this only works for atoms
                (if (member (car s1) s2 :test 'equal)
                    (rel-complement (cdr s1) s2);exclude
                    (cons (car s1) (rel-complement (cdr s1) s2));include
                )
            )  
        )
    )
)

; Prove that two sets are equivalent with the 
;   identity principal.
; If A-B={} AND B-A={} -> A = B
; If non-set lists are given as args they will
;   be treated as if they are sets.
; !!! Depends on rel-complement
; param A: First comparator set
; param B: Second comparator set
; return: Bool, are the sets equivalent?
(defun set-equal (s1 s2)
    (and 
         (null (rel-complement s1 s2))
         (null (rel-complement s2 s1))
    )
)

;----------------------

; Count the number of times that a pattern is matched
;   within an expression.
; param ele: The pattern to check within the expression
; param expr: The expression to match against
; return: the number of times the pattern is matched
(defun count-anywhere (ele expr)
    ;are there still memebers to check?
    (if (>= (length expr) 1)
        ;switch for types
        (cond 
            ;if both list
            ( (and (listp ele) (listp (car expr)))
                (if (equal ele (car expr)) 
                    ;lisp match, increment and continue to check lst
                    (+ 1 (count-anywhere ele (cdr expr)))
                    ;not equal, continue searching the car AND cdr
                    (+ (count-anywhere ele (car expr))
                        (count-anywhere ele (cdr expr))
                    )
                )

            )

            ;if both atom
            ( (and (atom ele) (atom (car expr)))
                (if (equal ele (car expr)) 
                    ;atom match, increment and continue to check
                    (+ 1 (count-anywhere ele (cdr expr)))
                    ;not equal, continue searching cdr
                    (+ 0 (count-anywhere ele (cdr expr)))
                )
            )

            ;if atom and list
            ( (and (atom ele) (list (car expr)))
                ;recurse into the list
                (+ (count-anywhere ele (car expr))
                    (count-anywhere ele (cdr expr))
                    )
            )

            ;if list and atom
            ( (and (list ele) (atom (car expr)))
                ;recurse into the cdr
                (count-anywhere ele (cdr expr))
            )
            ;default
            (t nil)
        );cond end
        ;else end, no more matches anywhere
        0
    )
)

;----------------------

; Count the number of cells in the expression. An expression
;   has one cell per non-nil member, and each of it's members'
;   members if they are lists.
; param expr: The expression to find the number of cells of
; return: The number of cells
(defun count-cells (expr)
    (cond  
        ;if atom
        ((atom expr)
            0
        )

        ;if list
        ((listp expr)
            
            ;if car is atom
            (if (atom (car expr))
                ; +1 and explore further TODO length?
                (+ 1 (count-cells (cdr expr)))

                ;else, car is a list
                (cond 
                    
                    ;if 2+ elements left, recurse into both 
                    ((>= (length expr) 2)
                        (+  1 
                            (+ (count-cells (car expr)) 
                                (count-cells (cdr expr)))
                        )
                    )

                    ;if one element left, recurse into it
                    ((= (length expr) 1)
                        (+ 1 (count-cells (car expr)))
                    )

                    ;if nil, return 0
                    (t 0)
                )
            )
        )
    )
)

;-------------------------------

; Determine the type of all atoms within an expression
; param s-exp: The expression to evaluate
; return: The s-exp with all of the atom values replaced with types
(defun typer (s-exp)
    ;if atomic,

    (if (null s-exp)

        ;don't include a nil
        nil

        ;if it's an atom
        (if (atom s-exp)

            ;determine atom type
            (if (numberp s-exp) 
                'NUMBER ;a number
                'SYMBOL ;a symbol
            )

            ;else it's a list
            ;recombine recursions on both sides
            (cons (typer (car s-exp)) (typer (cdr s-exp)))
            
        )
    )   
)

;-------------------------------

; Determines if a statement is true.
; param a: Question
; param b: List of truths
; return: Is this question true?
(defun query (q truth)
    ;have we exhausted the truths yet?
    (if (null truth)

        ;yes, return nil
        nil

        ;no, see if the q and car match
        (if (matchp q (car truth)) ;does it match?
            ;yes, return the matched truth
            (car truth)

            ;no, recurse
            (query q (cdr truth))
        )
    )
)

; A function to check if a two lists are equal in all positions.
; The universal symbol *var* will match anything in it's position.
; parem a: list a
; parem b: list b
; return: Bool, do these match?
(defun matchp (a b)
    ;if both atom
    (if (and (atom a) (atom b))
        ;check if they match
        (match-atoms a b)

        ;else, these are lists, match the cars and recurse on cdrs
        (and (match-atoms (car a) (car b)) (matchp (cdr a) (cdr b)))
    )
)

; A function to check if two atoms are equal, or if 
; one matches the universal symbol *var*.
; parem a: atom a
; parem b: atom b
; return: Bool, do these match?
(defun match-atoms (a b)
    (if ( or
            (equal a b)
            (or
                (equal a '*var*)
                (equal b '*var*)
            )
        )
        T
        nil
    )
)

;----------------------

; Return the attribute of an object, related to its property.
; Assumes that the object has only one or no parent object.
; param obj: The object to search on
; param prop: The property to search for
; return: The associated attribute, or nil if it doesn't exist
(defun inherit-get (obj prop)


    (if (not (null (get obj prop)))
        ;not null, return the valid

        (get obj prop)

        ;prop doesn't belond to the obj, do we have a parent?
        (if (not (null (get obj 'isa)))
            ;parent exists, recurse on it

            (if (atom (get obj 'isa))
                (inherit-get (get obj 'isa) prop)
                ;else we need to car it
                (inherit-get (car (get obj 'isa)) prop)
            )

            ;else no parent exists, return nil
            nil
        )
    )
)

;----------------------

; Return the attributes of an object matching
;   a property.
; Works with an indefinite number of parent objects.
; param obj: The object to check
; param prop: The property to search for
; return: The attribute associated with the property,
;   or nil if it doesn't exist
(defun multiple-inherit-get (obj prop)

    ;get requires an atom, is obj an atom?
    (if (atom obj)

        ;atom, check property
        (if (not (null (get obj prop)))

            ;we got it, return it
            (get obj prop)

            ;doesn't exist, do we have a parent?
            (if (not (null (get obj 'isa))) ;this excludes empty lists
           
                ;parent(s) exist, get them

                ;make sure we can use car
                (if (atom (get obj 'isa))
                        (multiple-inherit-get (get obj 'isa) prop)
                        
                        ;else we need to car it
                         (if (multiple-inherit-get (car (get obj 'isa)) prop);if it works,
                            
                            ;then use it and stop (this should REALLY be a variable)
                            (multiple-inherit-get (car (get obj 'isa)) prop)
                            
                            ;else recurse on the cutter
                            (multiple-inherit-get (cdr (get obj 'isa)) prop)
                         )
                 )
                ;else no parent exists, return nil
                nil
            )
        )

        ;list, gotta recurse
        (if  (multiple-inherit-get (car obj) prop)
            ;recurse onto the carriage, and the (nullable) cutter
            (multiple-inherit-get (car obj) prop)
            (multiple-inherit-get (cdr obj) prop)
        )        
    )
)