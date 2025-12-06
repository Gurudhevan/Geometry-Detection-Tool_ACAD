(defun c:ClusterHatches (/ tolerance refSelection refAreas avgArea 
                           allHatches resultSelection i hatch area)
  (princ "\n*** Hatch Clustering Program ***\n")
  
  ;; Get tolerance from user or use default
  (setq tolerance (getreal "\nEnter area tolerance [0.00100]: "))
  (if (null tolerance) (setq tolerance 0.00100))
  
  ;; Step 1: Prompt user to select reference hatches
  (setq refSelection (ssget ":L" '((0 . "HATCH"))))
  (if (null refSelection)
    (progn 
      (princ "\nError: No reference hatches selected.") 
      (princ)
      (exit)
    )
  )
  
  ;; Step 2: Extract areas from reference hatches
  (setq refAreas '())
  (setq i 0)
  (repeat (sslength refSelection)
    (setq hatch (ssname refSelection i))
    (if hatch
      (progn
        (vl-catch-all-apply
          '(lambda ()
            (setq area (vlax-get-property (vlax-ename->vla-object hatch) 'Area))
            (setq refAreas (cons area refAreas))
          )
        )
      )
    )
    (setq i (1+ i))
  )
  
  ;; Check if we got any valid areas
  (if (null refAreas)
    (progn
      (princ "\nError: Could not retrieve areas from the selected hatches.")
      (princ)
      (exit)
    )
  )
  
  ;; Calculate average area
  (setq avgArea (/ (apply '+ refAreas) (length refAreas)))
  (princ (strcat "\nReference area: " (rtos avgArea 2 6)))
  
  ;; Step 3: Cluster similar hatches
  (setq resultSelection (ssadd))
  (setq allHatches (ssget "X" '((0 . "HATCH"))))
  (if allHatches
    (progn
      (setq i 0)
      (repeat (sslength allHatches)
        (setq hatch (ssname allHatches i))
        (vl-catch-all-apply
          '(lambda ()
            (setq area (vlax-get-property (vlax-ename->vla-object hatch) 'Area))
            (if (<= (abs (- area avgArea)) tolerance)
              (ssadd hatch resultSelection)
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  
  ;; Step 4: Highlight clustered hatches
  (if (and resultSelection (> (sslength resultSelection) 0))
    (progn
      (sssetfirst nil resultSelection)
      (princ (strcat "\n" (itoa (sslength resultSelection)) " similar hatches found."))
    )
    (princ "\nNo similar hatches found.")
  )
  (princ)
)

(princ "\nLoaded: ClusterHatches command available.")