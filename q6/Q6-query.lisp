;author: Jesse Pingitore
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