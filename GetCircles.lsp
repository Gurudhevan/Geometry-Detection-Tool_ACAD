(defun c:ClusterCircles ()
  (princ "\n*** Circle Clustering Program ***\n")
  (setq tolerance 0.00300) ; Default tolerance for radius and area comparison

  ; Step 1: Prompt the user to select reference circles
  (setq refSelection (ssget ":L" '((0 . "CIRCLE"))))
  (if (not refSelection)
    (progn (princ "\nError: No reference circles selected.") (exit))
  )

  ; Function to get radius and area of a circle
  (defun GetCircleData (entity)
    (if (and entity (eq (cdr (assoc 0 (entget entity))) "CIRCLE"))
      (list (cdr (assoc 40 (entget entity))) ; Radius
            (* pi (expt (cdr (assoc 40 (entget entity))) 2))) ; Area
      '(0.0 0.0)
    )
  )

  ; Function to convert a 3D point to 2D
  (defun ConvertTo2D (pt)
    (list (car pt) (cadr pt))
  )

  ; Step 2: Extract radius and area from selected circles
  (setq refRadii '() refAreas '())
  (repeat (sslength refSelection)
    (setq circle (ssname refSelection 0))
    (setq data (GetCircleData circle))
    (setq refRadii (cons (car data) refRadii))
    (setq refAreas (cons (cadr data) refAreas))
    (ssdel circle refSelection)
  )

  ; Calculate average radius and area
  (setq avgRadius (/ (apply '+ refRadii) (length refRadii)))
  (setq avgArea (/ (apply '+ refAreas) (length refAreas)))

  ; Step 3: Cluster similar circles
  (setq resultSelection (ssadd))
  (setq allCircles (ssget "X" '((0 . "CIRCLE"))))
  (repeat (sslength allCircles)
    (setq circle (ssname allCircles 0))
    (setq circleObj (vlax-ename->vla-object circle))
    (setq data (GetCircleData circle))
    (setq centerPoint (ConvertTo2D (vlax-curve-getstartpoint circleObj)))
    (if (and (<= (abs (- (car data) avgRadius)) tolerance)
             (<= (abs (- (cadr data) avgArea)) tolerance))
      (ssadd circle resultSelection)
    )
    (ssdel circle allCircles)
  )

  ; Step 4: Highlight clustered circles
  (if (> (sslength resultSelection) 0)
    (progn
      (sssetfirst nil resultSelection)
      (princ (strcat "\n" (itoa (sslength resultSelection)) " similar circles clustered."))
    )
    (princ "\nNo similar circles found.")
  )
  (princ)
)

(princ "\nLoaded: ClusterCircles command available.")