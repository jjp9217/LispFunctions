;author: Jesse Pingitore
; Return the attributes of an object matching
;   a property.
; Works with an indefinite number of parent objects, but Assumes
; there are no cycles in the graph. THIS WILL FAIL IF SO.
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
                (if (multiple-inherit-get (car (get obj 'isa)) prop)
                    (multiple-inherit-get (car (get obj 'isa)) prop);then use it
                    (multiple-inherit-get (cdr (get obj 'isa)) prop);else recurse
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