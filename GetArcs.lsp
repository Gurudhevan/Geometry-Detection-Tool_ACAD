(defun c:ClusterArcs ()
  (princ "\n*** Arc Clustering Program ***\n")
  (setq lengthTolerance 0.00300) ; Default tolerance for arc length comparison
  (setq areaTolerance 0.00300) ; Default tolerance for area comparison

  ; Step 1: Prompt the user to select reference arcs
  (setq refSelection (ssget ":L" '((0 . "ARC"))))
  (if (not refSelection)
    (progn (princ "\nError: No reference arcs selected.") (exit))
  )

  ; Function to get length and area of an arc
  (defun GetArcData (entity)
    (if (and entity (eq (cdr (assoc 0 (entget entity))) "ARC"))
      (list (vlax-curve-getdistatparam entity (vlax-curve-getendparam entity))
            (vlax-curve-getarea entity))
      '(0.0 0.0)
    )
  )

  ; Step 2: Extract length and area from selected arcs
  (setq refLengths '())
  (setq refAreas '())
  (repeat (sslength refSelection)
    (setq arc (ssname refSelection 0))
    (setq data (GetArcData arc))
    (setq refLengths (cons (car data) refLengths))
    (setq refAreas (cons (cadr data) refAreas))
    (ssdel arc refSelection)
  )

  ; Calculate average length and area
  (setq avgLength (/ (apply '+ refLengths) (length refLengths)))
  (setq avgArea (/ (apply '+ refAreas) (length refAreas)))

  ; Step 3: Cluster similar arcs
  (setq resultSelection (ssadd))
  (setq allArcs (ssget "X" '((0 . "ARC"))))
  (repeat (sslength allArcs)
    (setq arc (ssname allArcs 0))
    (setq data (GetArcData arc))
    (if (and (<= (abs (- (car data) avgLength)) lengthTolerance)
             (<= (abs (- (cadr data) avgArea)) areaTolerance))
      (ssadd arc resultSelection)
    )
    (ssdel arc allArcs)
  )

  ; Step 4: Highlight clustered arcs
  (if (> (sslength resultSelection) 0)
    (progn
      (sssetfirst nil resultSelection)
      (princ (strcat "\n" (itoa (sslength resultSelection)) " similar arcs clustered."))
    )
    (princ "\nNo similar arcs found.")
  )
  (princ)
)

(princ "\nLoaded: ClusterArcs command available.")