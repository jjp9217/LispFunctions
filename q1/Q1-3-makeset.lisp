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
