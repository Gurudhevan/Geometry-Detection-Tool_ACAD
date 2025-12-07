(defun c:GetText ( / borderSel borderEnt borderObj numRows numCols minPt maxPt width height rowHeight colWidth i j x1 y1 x2 y2 ssGrid plineEnt plineObj plineLength ellipseObj ellipseArea allSmallObjs ssGroup gridLine action idx entType )

  (princ "\n--- GetText Routine Start ---")

  ;; Step 1: Select polygon boundary
  (prompt "\nSelect the polygon boundary:")
  (setq borderSel (ssget "_+.:E:S" '((0 . "LWPOLYLINE,POLYLINE"))))

  (if (not borderSel)
    (progn
      (prompt "\nNo polygon selected. Exiting.")
      (exit)
    )
  )

  ;; Get boundary object and its extents
  (setq borderEnt (ssname borderSel 0))
  (setq borderObj (vlax-ename->vla-object borderEnt))
  (vla-getboundingbox borderObj 'minPt 'maxPt)

  (setq minPt (vlax-safearray->list minPt)
        maxPt (vlax-safearray->list maxPt)
        width (- (car maxPt) (car minPt))
        height (- (cadr maxPt) (cadr minPt))
  )

  ;; Ask number of rows and columns
  (setq numRows (getint "\nEnter number of rows: "))
  (setq numCols (getint "\nEnter number of columns: "))

  (if (or (not numRows) (not numCols) (< numRows 1) (< numCols 1))
    (progn
      (prompt "\nInvalid number of rows/columns. Exiting.")
      (exit)
    )
  )

  ;; Calculate step size
  (setq rowHeight (/ height numRows)
        colWidth  (/ width  numCols)
  )

  ;; Step 2: Draw the grid lines
  (prompt "\nDrawing grid lines...")

  ;; Vertical grid lines
  (setq j 1)
  (while (< j numCols)
    (setq x (+ (car minPt) (* j colWidth)))
    (setq y1 (cadr minPt))
    (setq y2 (cadr maxPt))
    (setq gridLine (list (list x y1 0) (list x y2 0)))
    (entmakex (list (cons 0 "LINE") (cons 10 (nth 0 gridLine)) (cons 11 (nth 1 gridLine))))
    (setq j (1+ j))
  )

  ;; Horizontal grid lines
  (setq i 1)
  (while (< i numRows)
    (setq y (+ (cadr minPt) (* i rowHeight)))
    (setq x1 (car minPt))
    (setq x2 (car maxPt))
    (setq gridLine (list (list x1 y 0) (list x2 y 0)))
    (entmakex (list (cons 0 "LINE") (cons 10 (nth 0 gridLine)) (cons 11 (nth 1 gridLine))))
    (setq i (1+ i))
  )

  ;; Step 3: Loop through each grid and collect small objects
  (prompt "\nScanning grids for small polylines and ellipses...")
  (setq allSmallObjs '()) ; reset list

  (setq i 0)
  (while (< i numRows)
    (setq j 0)
    (while (< j numCols)
      ;; Grid window bounds
      (setq x1 (+ (car minPt) (* j colWidth)))
      (setq y1 (+ (cadr minPt) (* i rowHeight)))
      (setq x2 (+ x1 colWidth))
      (setq y2 (+ y1 rowHeight))

      ;; Select polylines and ellipses inside the grid
      (setq ssGrid (ssget "_C" (list x1 y1) (list x2 y2)
        '((-4 . "<OR")
          (0 . "LWPOLYLINE")
          (0 . "POLYLINE")
          (0 . "ELLIPSE")
          (-4 . "OR>")
        )))

      (if ssGrid
        (progn
          ;; Iterate selection set and filter based on length (polylines) and area (ellipses)
          (setq idx 0)
          (repeat (sslength ssGrid)
            (setq plineEnt (ssname ssGrid idx))
            (setq entType (cdr (assoc 0 (entget plineEnt))))

            (cond
              ;; For polylines: check length ≤ 0.3mm
              ((or (equal entType "LWPOLYLINE") (equal entType "POLYLINE"))
                (setq plineObj (vlax-ename->vla-object plineEnt))
                (setq plineLength (vlax-get plineObj 'Length))

                (if (<= plineLength 0.3) ; 0.3mm filter
                  (setq allSmallObjs (cons plineEnt allSmallObjs))
                )
              )

              ;; For ellipses: check area ≤ 0.1mm²
              ((equal entType "ELLIPSE")
                (setq ellipseObj (vlax-ename->vla-object plineEnt))
                (setq ellipseArea (vlax-get ellipseObj 'Area))

                (if (<= ellipseArea 1) ; 0.1mm² filter
                  (setq allSmallObjs (cons plineEnt allSmallObjs))
                )
              )
            )

            (setq idx (1+ idx))
          )
        )
      )

      (setq j (1+ j))
    )
    (setq i (1+ i))
  )

  ;; Step 4: Create selection set with all small objects
  (if allSmallObjs
    (progn
      ;; Create selection set
      (setq ssGroup (ssadd))
      (foreach e allSmallObjs
        (ssadd e ssGroup)
      )

      ;; Preselect the group
      (sssetfirst nil ssGroup)

      (prompt (strcat "\nTotal small objects found: " (itoa (sslength ssGroup))))
      (prompt "\nSmall polylines and ellipses preselected.")
    )
    (prompt "\nNo small polylines or ellipses found in any grid.")
  )

  ;; Step 5: Ask user to continue or quit
  (initget "Continue Quit")
  (setq action (getkword "\nType 'Continue' to proceed and run PDFSHXTEXT, or 'Quit' to stop: "))

  (if (= action "Continue")
    (progn
      (prompt "\nPreselection is active.")
      (prompt "\nNow manually run the PDFSHXTEXT command.")
      (prompt "\nNOTE: After PDFSHXTEXT, you may use 'SELECTPREVIOUS' to reselect the resulting text or objects.")
    )
    (prompt "\nProcess ended by user.")
  )

  (prompt "\n--- GetText Routine Completed ---")
  (princ)
)
