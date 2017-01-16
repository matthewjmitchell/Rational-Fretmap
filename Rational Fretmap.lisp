;matt mitchell 2014, updated 2016
;mattmitchellguitars@gmail.com

;Rational Fretmap - for designing microtonal fretboards given a tuning expressed in ratios
;returns x and y coordinates for fret centers
;
;arguments:
;a tuning system expressed as a list of ratios
;scale length of instrument from nut to saddle
;tuning of open strings expressed as a list of ratios
;length of fretboard
;string spacing, expressed as list of 2 item lists, where 2 items are, for each string, y coordinates of string at nut and saddle.
;optional initial nut compensation. 0.5mm recommended. 

(defun rational-fretmap (ratio-list scale-length string-ratio-list fingerboard-length string-spacing-list &optional (nut-compensation 0))
  "Lists coordinates for fret centers. First element in each list x value (nut distance). Subsequent elements are y values for each string"
  (sort-nut-distance-list-six-strings 
   (list-all-distances 
    (multi-string-rational-fretmap ratio-list scale-length string-ratio-list fingerboard-length nut-compensation))
   (multi-string-rational-fretmap ratio-list scale-length string-ratio-list fingerboard-length nut-compensation)
   string-spacing-list
   scale-length))
;(rational-fretmap '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8) 650 '(3/2 1/1 4/3 9/5 9/8 3/2) 480 '((-15 -21)(-10 -14)(-5 -7)(5 7)(10 14)(15 21)))
;(rational-fretmap '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8) 650 '(3/2 1/1 4/3 9/5 9/8 3/2) 480 '((-15 -21)(-10 -14)(-5 -7)(5 7)(10 14)(15 21)) 0.5)

;outputs sorted-nut-distance-list in format:
;((x1 1 2 6)(x2 3 4 5)(x3 2 3 4)(x4 1 5 6)...)
;where x = a distance from nut, and following numbers indicate strings which have a fret there
;strings are numbered from bass to treble. Make this flexible for different numbers of strings.
;calls the functions: (find-center-of-string and (string-slope
(defun sort-nut-distance-list-six-strings (list-of-all-distances nut-distance-list string-spacing-list scale-length)
  "returns list of fret locations with numbers indicating which strings have a fret at that location"
  (if (null list-of-all-distances) ()
    (cons
     (list 
      (first list-of-all-distances)
      (if (search (list (first list-of-all-distances)) (first nut-distance-list))
        (find-center-of-string (first list-of-all-distances) (first (first string-spacing-list)) (second (first string-spacing-list)) scale-length)) ;put some recursion here, so it can work with any number of strings
      (if (search (list (first list-of-all-distances)) (second nut-distance-list))
        (find-center-of-string (first list-of-all-distances) (first (second string-spacing-list)) (second (second string-spacing-list)) scale-length))
      (if (search (list (first list-of-all-distances)) (third nut-distance-list))
        (find-center-of-string (first list-of-all-distances) (first (third string-spacing-list)) (second (third string-spacing-list)) scale-length))
      (if (search (list (first list-of-all-distances)) (fourth nut-distance-list)) 
        (find-center-of-string (first list-of-all-distances) (first (fourth string-spacing-list)) (second (fourth string-spacing-list)) scale-length))
      (if (search (list (first list-of-all-distances)) (fifth nut-distance-list)) 
        (find-center-of-string (first list-of-all-distances) (first (fifth string-spacing-list)) (second (fifth string-spacing-list)) scale-length))
      (if (search (list (first list-of-all-distances)) (sixth nut-distance-list)) 
        (find-center-of-string (first list-of-all-distances) (first (sixth string-spacing-list)) (second (sixth string-spacing-list)) scale-length)))
     (sort-nut-distance-list-six-strings (rest list-of-all-distances) nut-distance-list string-spacing-list scale-length))))
;replace this with a recursive function that will allow for any number of strings

(defun find-center-of-string (distance-from-nut center-at-nut center-at-saddle scale-length)
  "finds distance from centerline of fingerboard to center of string for the given distance from nut"
  (+ (*  (string-slope center-at-nut center-at-saddle scale-length) distance-from-nut) center-at-nut))
;(find-center-of-string 40.625 -20.87 -24.87 650)

;outputs nut-distance-list
(defun multi-string-rational-fretmap (ratio-list scale-length string-ratio-list fingerboard-length &optional (nut-compensation 0))
"returns fretting distances for more than one string"
  (if (null string-ratio-list) ()
    (cons (one-string-rational-fretmap ratio-list scale-length (first string-ratio-list) fingerboard-length nut-compensation)
          (multi-string-rational-fretmap ratio-list scale-length (rest string-ratio-list) fingerboard-length nut-compensation)
          )))
;(multi-string-rational-fretmap '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8) 650 '(3/2 1/1 4/3 9/5 9/8 3/2) 475)
;(multi-string-rational-fretmap '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8) 650 '(3/2 1/1 4/3 9/5 9/8 3/2) 475 0.5)

(defun one-string-rational-fretmap (ratio-list scale-length string-ratio fingerboard-length &optional (nut-compensation 0) (ratio-count 0) (cycle-count 1))
  "returns fretting distances for one string"
  ;exit condition: end of fingerboard
  (if (>= (rational-fret-locator (* cycle-count (nth ratio-count ratio-list)) scale-length string-ratio) fingerboard-length) ()
    ;find the first ratio that is greater than string-ratio
    (if (<= (* cycle-count (nth ratio-count ratio-list)) string-ratio)
      ;if ratio is not greater than string-ratio, recurse and increment ratio-count
      (one-string-rational-fretmap ratio-list scale-length string-ratio fingerboard-length nut-compensation (1+ ratio-count) cycle-count)
      ;make the list of distances
      (cons (- (rational-fret-locator (* cycle-count (nth ratio-count ratio-list)) scale-length string-ratio) nut-compensation)
           ;recurse
            (one-string-rational-fretmap ratio-list scale-length string-ratio fingerboard-length nut-compensation
                                         ;cycle through the ratio-list, starting again at position 0 when you reach the end
                                         (if (= ratio-count (1- (length ratio-list))) 0 (1+ ratio-count))
                                         ;for each time through the ratio-list, increment cycle-count
                                         (if (= ratio-count (1- (length ratio-list))) (* 2 cycle-count) cycle-count)
                                         )))))
;(one-string-rational-fretmap '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8) 650 3/2 475)
;(one-string-rational-fretmap '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8) 650 3/2 475 0.5)

(defun rational-fret-locator (ratio scale-length &optional (string-ratio 1/1))
"returns fretting distance from nut for the given ratio"
  (float (- scale-length (* (/ 1 (/ ratio (octave-reduce string-ratio))) scale-length))))
;(rational-fret-locator 3/1 650 3/4)

;outputs list-of-all-distances
(defun list-all-distances (nut-distance-list)
  "converts list of fret distances per string into a single list of all fret distances"
    (sort (remove-duplicates (apply #'append nut-distance-list)) #'<))
;(list-all-distances (multi-string-rational-fretmap '(1/1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8) 650 '(3/2 1/1 4/3 9/5 9/8 3/2) 475))

(defun octave-reduce (ratio)
"takes a ratio and expresses it as between 1/1 and (not including) 2/1"
(if (< ratio 1)
  (octave-reduce (* ratio 2))
  (if (>= ratio 2)
    (octave-reduce (/ ratio 2))
    ratio)))

;spacing and taper;

;nut = 0 on the x-axis
;fingerboard centerline = 0 on y-axis
;y = mx + b
;m = (y2 - y1) / (x2 - x1)

;center-at-nut is distance from centerline at the nut 
;center-at-saddle is distance from centerline at the saddle
;point one = 0, center-at-nut
;point two = scale-length, center-at-saddle
;slope = (center-at-saddle - center-at-nut) / (scale-length - 0)
;or
;slope = (center-at-saddle - center-at-nut) / scale-length
(defun string-slope (center-at-nut center-at-saddle scale-length)
  "finds slope of a string in relation to nut (x-axis) and fingerboard centerline (y-axis)"
  (/ (- center-at-saddle center-at-nut) scale-length))
;(string-slope -20.87 -24.87 650)
  
;y = mx + b
;y is distance from centerline
;x is distance from nut 
;m is slope
;b is y-intercept, aka center-at-nut 
;to find y:
;y = (slope * distance-from-nut) + center-at-nut

(defun find-center-of-string (distance-from-nut center-at-nut center-at-saddle scale-length)
  "finds distance from centerline of fingerboard to center of string for the given distance from nut"
  (+ (*  (string-slope center-at-nut center-at-saddle scale-length) distance-from-nut) center-at-nut))
;(find-center-of-string 40.625 -20.87 -24.87 650)

;string-spacing-list format:
;((center-at-nut center-at-saddle)(center-at-nut center-at-saddle)...)
;center-at-nut is the center of string at nut
;center-at-saddle is center of string at saddle
;strings are listed from bass to treble
;all are given as coordinates measured from centerline, negative on the bass side, positive on the treble side
;i.e. pairs of y coordinates, the first associated with x = 0 (nut), the second associated with x = scale-length (saddle)
;((-15 -21)(-10 -14)(-5 -7)(5 7)(10 14)(15 21))

;fingerboard-taper-list is y coordinates (from centerline) of fingerboard edge at nut and half-scale-length (12th fret)
;in format:
;((bass-edge-at-nut bass-edge-at-saddle)(treble-edge-at-nut treble-edge-at-saddle))
;(-20 -24)(20 24)



  
  

          