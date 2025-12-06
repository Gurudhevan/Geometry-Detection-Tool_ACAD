(defun c:ClusterEllipses ()
  (princ "\n*** Ellipse Clustering Program ***\n")
  (setq tolerance 0.00300) ; Default tolerance for area comparison

  ; Step 1: Prompt the user to select reference ellipses
  (setq refSelection (ssget ":L" '((0 . "ELLIPSE"))))
  (if (not refSelection)
    (progn (princ "\nError: No reference ellipses selected.") (exit))
  )

  ; Function to get area of an ellipse
  (defun GetEllipseData (entity)
    (if (and entity (eq (cdr (assoc 0 (entget entity))) "ELLIPSE"))
      (progn
        (setq ellipseObj (vlax-ename->vla-object entity))
        (list (vla-get-area ellipseObj)))
      '(0.0)
    )
  )

  ; Step 2: Extract area from selected ellipses
  (setq refAreas '())
  (repeat (sslength refSelection)
    (setq ellipse (ssname refSelection 0))
    (setq data (GetEllipseData ellipse))
    (setq refAreas (cons (nth 0 data) refAreas))
    (ssdel ellipse refSelection)
  )

  ; Calculate average area
  (setq avgArea (/ (apply '+ refAreas) (length refAreas)))

  ; Step 3: Cluster similar ellipses
  (setq resultSelection (ssadd))
  (setq allEllipses (ssget "X" '((0 . "ELLIPSE"))))
  (repeat (sslength allEllipses)
    (setq ellipse (ssname allEllipses 0))
    (setq data (GetEllipseData ellipse))
    (if (<= (abs (- (nth 0 data) avgArea)) tolerance)
      (ssadd ellipse resultSelection)
    )
    (ssdel ellipse allEllipses)
  )

  ; Step 4: Highlight clustered ellipses
  (if (> (sslength resultSelection) 0)
    (progn
      (sssetfirst nil resultSelection)
      (princ (strcat "\n" (itoa (sslength resultSelection)) " similar ellipses clustered."))
    )
    (princ "\nNo similar ellipses found.")
  )
  (princ)
)

(princ "\nLoaded: ClusterEllipses command available.")