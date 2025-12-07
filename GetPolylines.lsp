; Polyline Clustering AutoLISP Script
; Clusters polylines based on user selection and boundary
; Uses length and area for clustering within a user-defined boundary

(defun c:ClusterPolylines ()
  (princ "\n*** Polyline Clustering Program ***\n")
  ;; Get tolerance from user or use default
  (setq tolerance (getreal "\nEnter area tolerance [0.00300]: "))
  (if (null tolerance) (setq tolerance 0.00300))

  ; Step 1: Prompt the user to select a boundary
  (setq boundary (car (entsel "\nSelect the boundary polyline: ")))
  (if (not boundary)
    (progn (princ "\nError: No boundary selected.") (exit))
  )

  ; Step 2: Prompt the user to select reference polylines
  (setq refSelection (ssget ":L" '((0 . "LWPOLYLINE"))))
  (if (not refSelection)
    (progn (princ "\nError: No reference polylines selected.") (exit))
  )

  ; Function to get length and area of a polyline
  (defun GetPolylineData (entity)
    (if (and entity (eq (cdr (assoc 0 (entget entity))) "LWPOLYLINE"))
      (list (vlax-curve-getdistatparam entity (vlax-curve-getendparam entity))
            (vlax-curve-getarea entity))
      '(0.0 0.0)
    )
  )

  ; Function to convert a 3D point to 2D
  (defun ConvertTo2D (pt)
    (list (car pt) (cadr pt))
  )

  ; Function to check if a point is inside a polyline boundary
  (defun PointInPolyline (pt poly)
    (setq inside nil)
    (setq vertices (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates poly))))
    (setq num-verts (/ (length vertices) 2))
    (setq i 0)
    (setq j (- num-verts 1))
    (while (< i num-verts)
      (setq xi (nth (* 2 i) vertices)
            yi (nth (+ 1 (* 2 i)) vertices)
            xj (nth (* 2 j) vertices)
            yj (nth (+ 1 (* 2 j)) vertices))
      (if (and (/= yi yj)
               (< (min yi yj) (cadr pt) (max yi yj))
               (< (car pt) (+ xi (* (/ (- xj xi) (- yj yi)) (- (cadr pt) yi)))))
        (setq inside (not inside))
      )
      (setq j i)
      (setq i (1+ i))
    )
    inside
  )

  ; Step 3: Extract length and area from selected polylines
  (setq refLengths '() refAreas '())
  (repeat (sslength refSelection)
    (setq poly (ssname refSelection 0))
    (setq data (GetPolylineData poly))
    (setq refLengths (cons (car data) refLengths))
    (setq refAreas (cons (cadr data) refAreas))
    (ssdel poly refSelection)
  )

  ; Calculate average length and area
  (setq avgLength (/ (apply '+ refLengths) (length refLengths)))
  (setq avgArea (/ (apply '+ refAreas) (length refAreas)))

  ; Step 4: Cluster similar polylines within the boundary
  (setq resultSelection (ssadd))
  (setq allPolylines (ssget "X" '((0 . "LWPOLYLINE"))))
  (repeat (sslength allPolylines)
    (setq poly (ssname allPolylines 0))
    (setq polyObj (vlax-ename->vla-object poly))
    (setq data (GetPolylineData poly))
    (setq startPoint (ConvertTo2D (vlax-curve-getstartpoint polyObj)))
    (if (and (<= (abs (- (car data) avgLength)) tolerance)
             (<= (abs (- (cadr data) avgArea)) tolerance)
             (PointInPolyline startPoint (vlax-ename->vla-object boundary))
        )
      (ssadd poly resultSelection)
    )
    (ssdel poly allPolylines)
  )

  ; Step 5: Highlight clustered polylines
  (if (> (sslength resultSelection) 0)
    (progn
      (sssetfirst nil resultSelection)
      (princ (strcat "\n" (itoa (sslength resultSelection)) " similar polylines clustered."))
    )
    (princ "\nNo similar polylines found.")
  )
  (princ)
)

(princ "\nLoaded: ClusterPolylines command available.")
