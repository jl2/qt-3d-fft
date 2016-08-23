;;;; qt-3d-fft.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:qt-3d-fft)
(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 3) (safety 1) (size 0) (debug 0)))

(defparameter *fps* 60)
(defparameter *fft-window-size* (* 512 2))

;; map-val is used to map logical coordinates to screen coordinates.
(declaim (inline map-val epitrochoid-x epitrochoid-y hypotrochoid-x hypotrochoid-y))
(defun map-val (x xmin xmax new-xmin new-xmax)
  "Map a value from the range xmin,xmax to the range new-xmin,new-xmax"
  (+ (* (/ (- x xmin) (- xmax xmin)) (- new-xmax new-xmin)) new-xmin))

(defun epitrochoid-x (a b h tv)
  "X component of the parametric equation for an epitrochoid curve."
  (- (* (+ a b) (cos tv)) (* h (cos (* tv (/ (+ a b) b))))))

(defun epitrochoid-y (a b h tv)
  "Y component of the parametric equation for an epitrochoid curve."
  (- (* (+ a b) (sin tv)) (* h (sin (* tv (/ (+ a b) b))))))

(defun hypotrochoid-x (a b h tv)
  "X component of the parametric equation for an hypotrochoid curve."
  (+ (* (- a b) (cos tv)) (* h (cos (* tv (/ (- a b) b))))))

(defun hypotrochoid-y (a b h tv)
  "Y component of the parametric equation for an hypotrochoid curve."
  (+ (* (- a b) (sin tv)) (* h (sin (* tv (/ (- a b) b))))))


(define-widget spirograph-animator (QGLWidget)
               ((the-mp3 :initform nil)
                (start-time :initform 0)
                (song-duration :initform 0)

                (steps :initform 480)

                ;; TODO: Make this a structure like in spiro-animation
                (a-val :initform 36.0)
                (a-low :initform 1)
                (a-high :initform 2)
                (a-scale :initform 0.00010)
                (a-offset :initform 0.0)
                
                (b-val :initform 7.0)
                (b-low :initform 3)
                (b-high :initform 4)
                (b-scale :initform 0.00010)
                (b-offset :initform 0.0)

                (h-val :initform 22.0)
                (h-low :initform 5)
                (h-high :initform 6)
                (h-scale :initform 0.0010)
                (h-offset :initform 0.0)

                (dt-1 :initform 8.010)
                (dt-1-low :initform 7)
                (dt-1-high :initform 8)
                (dt-1-scale :initform 0.00010)
                (dt-1-offset :initform 0.0)

                (dt-2 :initform 8.001)
                (dt-2-low :initform 9)
                (dt-2-high :initform 10)
                (dt-2-scale :initform 0.00010)
                (dt-2-offset :initform 0.0)

                (dt-type :initform :over-pi)

                (x-function :initform #'epitrochoid-x)
                (y-function :initform #'epitrochoid-y))
               (:documentation "The spirograh-drawer widget draws an epitrochoid or hyptrochoid curve using the currently specified parameters."))

(define-subwidget (spirograph-animator timer) (q+:make-qtimer spirograph-animator)
  (setf (q+:single-shot timer) nil))

(define-initializer (spirograph-animator setup)
  (q+:start timer (round (/ 1000 *fps*)))
  (setf (q+:auto-fill-background spirograph-animator) nil))

(define-slot (spirograph-animator tick) ()
  (declare (connected timer (timeout)))
  (q+:repaint spirograph-animator))

(define-override (spirograph-animator initialize-G-L) ()
  (gl:clear-color 0 0 0 1)
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test))

(define-override (spirograph-animator resize-g-l) (width height)

  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -1.0 1.0 -1.0 1.0 -1.0 1.0))

;; (glu:perspective 50 (/ height width) 1.0 5000.0)
  ;; (glu:look-at 1200 1200 1200 
  ;;              0 0 0
  ;;              0 1 0))

(declaim (inline compute-offset))
(defun compute-offset (current scale low high left-fft-data right-fft-data)
  (if (= high low)
      current
      (let ((rh (if (< high low ) low high))
            (rl (if (< high low ) high low)))
        (+ current
           (loop for idx from rl below rh
              summing (aref left-fft-data idx) into total
              finally (return (* scale (/ (abs total) (- rh rl)))))))))

(define-override (spirograph-animator paint-g-l paint) ()
  "Handle paint events."
  (let* ((max-radius (* 1.1 (+ a-offset a-val b-offset b-val h-offset h-val)))
         (width (q+:width spirograph-animator))
         (height (q+:height spirograph-animator))
         (x-aspect-ratio (if (< height width)
                             (/ height width 1.0)
                             1.0))
         (y-aspect-ratio (if (< height width)
                             1.0
                             (/ width height 1.0)))
         (rdt-1 (if (eql dt-type :normal)
                    (+ dt-1-offset dt-1)
                    (/ pi (+ dt-1-offset dt-1)))))

    ;; Define some local functions for convenience
    (flet (
           (declare (inline xmapper ymapper spirograph-x spirograph-y))

           ;; xmapper maps logical x coordinates in the range x-min to x-max to screen coordinates in the range 0 to width
           (xmapper (x) (map-val (* x-aspect-ratio x) (- max-radius) max-radius -1.0 1.0))

           ;; ymapper does the same thing, but for y coordinates
           (ymapper (y) (map-val (* y-aspect-ratio y) (- max-radius) max-radius -1.0 1.0))
           
           ;; spirograph-x and spirograph-y hide funcall and make the code easier to read below
           (spirograph-x (a b h tv) (funcall x-function a b h tv))
           (spirograph-y (a b h tv) (funcall y-function a b h tv)))
      (with-finalizing 
          ;; Create a painter object to draw on
          ((painter (q+:make-qpainter spirograph-animator)))

        (gl:matrix-mode :modelview)
        (gl:load-identity)

        (gl:clear :color-buffer :depth-buffer)
        ;; Clear the background
        (when (and the-mp3 (< (/ (- (get-internal-real-time) start-time) 1.0 internal-time-units-per-second) song-duration))

          (let* ((height (q+:height spirograph-animator))
                 (width (q+:width spirograph-animator))

                 (location (/ (+ 10 (- (get-internal-real-time) start-time)) 1.0 internal-time-units-per-second))
                 (win-center (ceiling (max 0 
                                           (- (* 44100 location)
                                              (round (/ *fft-window-size* 2))))))

                 (left-fft-data (bordeaux-fft:windowed-fft (mp3-file-left-channel the-mp3) win-center *fft-window-size*))
                 (right-fft-data (bordeaux-fft:windowed-fft (mp3-file-right-channel the-mp3) win-center *fft-window-size*)))
            
            ;; Actual drawing goes here.  In this case, just a line.
            (gl:push-matrix)


            ;; TODO: Use "modern" OpenGL
            (gl:with-primitives :lines
              (gl:color 1 0 0)
              (loop
                 for i below steps
                 for cur-t = 0.0 then (* i rdt-1)
                 do
                   (let ((r-a (+ a-val a-offset))
                         (r-b (+ b-val b-offset))
                         (r-h (+ h-val h-offset))
                         (r-dt-2 (if (eql dt-type :normal)
                                     (+ dt-2 dt-2-offset)
                                     (/ pi (+ dt-2 dt-2-offset)))))

                     (gl:vertex (xmapper (spirograph-x r-a r-b r-h cur-t))
                                (ymapper (spirograph-y r-a r-b r-h cur-t))
                                0.0)
                     (gl:vertex (xmapper (spirograph-x r-a r-b r-h (+ r-dt-2 cur-t)))
                                (ymapper (spirograph-y r-a r-b r-h (+ r-dt-2 cur-t)))
                                0.0))))

            ;; Update offsets based on fft-data
            (setf a-offset (compute-offset a-offset a-scale a-low a-high left-fft-data right-fft-data))
            (setf b-offset (compute-offset b-offset b-scale b-low b-high left-fft-data right-fft-data))
            (setf h-offset (compute-offset h-offset h-scale h-low h-high left-fft-data right-fft-data))
            (setf dt-1-offset (compute-offset dt-1-offset dt-1-scale dt-1-low dt-1-high left-fft-data right-fft-data))
            (setf dt-2-offset (compute-offset dt-2-offset dt-2-scale dt-2-low dt-2-high left-fft-data right-fft-data))

            (gl:pop-matrix)))))))

(define-widget spirograph-widget (QWidget)
               ()
               (:documentation "A Spirograph animator and its controls."))

(define-subwidget (spirograph-widget fft-viewer) (make-instance 'spirograph-animator)
  "The spirograph-drawer itself.")


;; TODO: Use macros or something to make this less repetitive
(define-subwidget (spirograph-widget a-val-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'a' value spinbox."
  (q+:set-decimals a-val-spin 5)
  (q+:set-single-step a-val-spin 0.1)
  (q+:set-maximum a-val-spin 10000.0)
  (q+:set-minimum a-val-spin 0.00001)
  (q+:set-value a-val-spin (slot-value fft-viewer 'a-val)))

(define-subwidget (spirograph-widget a-scale-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'a-scale' value spinbox."
  (q+:set-decimals a-scale-spin 5)
  (q+:set-single-step a-scale-spin 0.1)
  (q+:set-maximum a-scale-spin 10000.0)
  (q+:set-minimum a-scale-spin 0.00001)
  (q+:set-value a-scale-spin (slot-value fft-viewer 'a-scale)))

(define-subwidget (spirograph-widget a-low-spin) (q+:make-qspinbox spirograph-widget)
  "The 'a-low' value spinbox."
  (q+:set-maximum a-low-spin *fft-window-size*)
  (q+:set-minimum a-low-spin 1)
  (q+:set-value a-low-spin (slot-value fft-viewer 'a-low)))

(define-subwidget (spirograph-widget a-high-spin) (q+:make-qspinbox spirograph-widget)
  "The 'a-high' value spinbox."
  (q+:set-maximum a-high-spin *fft-window-size*)
  (q+:set-minimum a-high-spin 1)
  (q+:set-value a-high-spin (slot-value fft-viewer 'a-high)))


(define-subwidget (spirograph-widget b-val-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'b' value spinbox."
  (q+:set-decimals b-val-spin 5)
  (q+:set-single-step b-val-spin 0.1)
  (q+:set-maximum b-val-spin 10000.0)
  (q+:set-minimum b-val-spin 0.00001)
  (q+:set-value b-val-spin (slot-value fft-viewer 'b-val)))

(define-subwidget (spirograph-widget b-scale-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'b-scale' value spinbox."
  (q+:set-decimals b-scale-spin 5)
  (q+:set-single-step b-scale-spin 0.1)
  (q+:set-maximum b-scale-spin 10000.0)
  (q+:set-minimum b-scale-spin 0.00001)
  (q+:set-value b-scale-spin (slot-value fft-viewer 'b-scale)))

(define-subwidget (spirograph-widget b-low-spin) (q+:make-qspinbox spirograph-widget)
  "The 'b-low' value spinbox."
  (q+:set-maximum b-low-spin *fft-window-size*)
  (q+:set-minimum b-low-spin 1)
  (q+:set-value b-low-spin (slot-value fft-viewer 'b-low)))

(define-subwidget (spirograph-widget b-high-spin) (q+:make-qspinbox spirograph-widget)
  "The 'b-high' value spinbox."
  (q+:set-maximum b-high-spin *fft-window-size*)
  (q+:set-minimum b-high-spin 1)
  (q+:set-value b-high-spin (slot-value fft-viewer 'b-high)))



(define-subwidget (spirograph-widget h-val-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'h' value spinbox."
  (q+:set-decimals h-val-spin 5)
  (q+:set-single-step h-val-spin 0.1)
  (q+:set-maximum h-val-spin 10000.0)
  (q+:set-minimum h-val-spin 0.00001)
  (q+:set-value h-val-spin (slot-value fft-viewer 'h-val)))

(define-subwidget (spirograph-widget h-scale-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'h-scale' value spinbox."
  (q+:set-decimals h-scale-spin 5)
  (q+:set-single-step h-scale-spin 0.1)
  (q+:set-maximum h-scale-spin 10000.0)
  (q+:set-minimum h-scale-spin 0.00001)
  (q+:set-value h-scale-spin (slot-value fft-viewer 'h-scale)))

(define-subwidget (spirograph-widget h-low-spin) (q+:make-qspinbox spirograph-widget)
  "The 'h-low' value spinbox."
  (q+:set-maximum h-low-spin *fft-window-size*)
  (q+:set-minimum h-low-spin 1)
  (q+:set-value h-low-spin (slot-value fft-viewer 'h-low)))

(define-subwidget (spirograph-widget h-high-spin) (q+:make-qspinbox spirograph-widget)
  "The 'h-high' value spinbox."
  (q+:set-maximum h-high-spin *fft-window-size*)
  (q+:set-minimum h-high-spin 1)
  (q+:set-value h-high-spin (slot-value fft-viewer 'h-high)))


(define-subwidget (spirograph-widget dt-1-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'dt-1' value spinbox."
  (q+:set-decimals dt-1-spin 5)
  (q+:set-single-step dt-1-spin 0.1)
  (q+:set-minimum dt-1-spin 0.00001)
  (q+:set-maximum dt-1-spin (* pi 1000))
  (q+:set-value dt-1-spin (slot-value fft-viewer 'dt-1)))

(define-subwidget (spirograph-widget dt-1-scale-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'dt-1-scale' value spinbox."
  (q+:set-decimals dt-1-scale-spin 5)
  (q+:set-single-step dt-1-scale-spin 0.1)
  (q+:set-maximum dt-1-scale-spin 10000.0)
  (q+:set-minimum dt-1-scale-spin 0.00001)
  (q+:set-value dt-1-scale-spin (slot-value fft-viewer 'dt-1-scale)))

(define-subwidget (spirograph-widget dt-1-low-spin) (q+:make-qspinbox spirograph-widget)
  "The 'dt-1-low' value spinbox."
  (q+:set-maximum dt-1-low-spin *fft-window-size*)
  (q+:set-minimum dt-1-low-spin 1)
  (q+:set-value dt-1-low-spin (slot-value fft-viewer 'dt-1-low)))

(define-subwidget (spirograph-widget dt-1-high-spin) (q+:make-qspinbox spirograph-widget)
  "The 'dt-1-high' value spinbox."
  (q+:set-maximum dt-1-high-spin *fft-window-size*)
  (q+:set-minimum dt-1-high-spin 1)
  (q+:set-value dt-1-high-spin (slot-value fft-viewer 'dt-1-high)))


(define-subwidget (spirograph-widget dt-2-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'dt-2' value spinbox."
  (q+:set-decimals dt-2-spin 5)
  (q+:set-single-step dt-2-spin 0.1)
  (q+:set-minimum dt-2-spin 0.00001)
  (q+:set-maximum dt-2-spin (* pi 1000))
  (q+:set-value dt-2-spin (slot-value fft-viewer 'dt-2)))

(define-subwidget (spirograph-widget dt-2-scale-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'dt-2-scale' value spinbox."
  (q+:set-decimals dt-2-scale-spin 5)
  (q+:set-single-step dt-2-scale-spin 0.1)
  (q+:set-maximum dt-2-scale-spin 10000.0)
  (q+:set-minimum dt-2-scale-spin 0.00001)
  (q+:set-value dt-2-scale-spin (slot-value fft-viewer 'dt-2-scale)))

(define-subwidget (spirograph-widget dt-2-low-spin) (q+:make-qspinbox spirograph-widget)
  "The 'dt-2-low' value spinbox."
  (q+:set-maximum dt-2-low-spin *fft-window-size*)
  (q+:set-minimum dt-2-low-spin 1)
  (q+:set-value dt-2-low-spin (slot-value fft-viewer 'dt-2-low)))

(define-subwidget (spirograph-widget dt-2-high-spin) (q+:make-qspinbox spirograph-widget)
  "The 'dt-2-high' value spinbox."
  (q+:set-maximum dt-2-high-spin *fft-window-size*)
  (q+:set-minimum dt-2-high-spin 1)
  (q+:set-value dt-2-high-spin (slot-value fft-viewer 'dt-2-high)))


(define-subwidget (spirograph-widget epitrochoid-button) (q+:make-qradiobutton "Epitrochoid")
  "Epitrochoid radio button."
  (q+:set-checked epitrochoid-button t))

(define-subwidget (spirograph-widget hypotrochoid-button) (q+:make-qradiobutton "Hypotrochoid")
  "Hypotrochoid radio button.")

(define-subwidget (spirograph-widget button-group) (q+:make-qbuttongroup spirograph-widget)
  "Button group to ensure radio buttons are exclusive."
  (q+:set-exclusive button-group t)
  (q+:add-button button-group epitrochoid-button)
  (q+:add-button button-group hypotrochoid-button))

(define-subwidget (spirograph-widget steps-spin) (q+:make-qspinbox spirograph-widget)
  "The spinbox for the number of steps."
  (q+:set-maximum steps-spin 10000000)
  (q+:set-minimum steps-spin 4)
  (q+:set-value steps-spin (slot-value fft-viewer 'steps)))


(define-subwidget (spirograph-widget use-dt-button) (q+:make-qradiobutton "dt = dt")
  "dt = dt")

(define-subwidget (spirograph-widget pi-over-dt-button) (q+:make-qradiobutton "dt = π/dt (Symetric)")
  "dt = π/dt"
  (q+:set-checked pi-over-dt-button t))

(define-subwidget (spirograph-widget dt-button-group) (q+:make-qbuttongroup spirograph-widget)
  "Button group to ensure radio buttons are exclusive."
  (q+:set-exclusive dt-button-group t)
  (q+:add-button dt-button-group use-dt-button)
  (q+:add-button dt-button-group pi-over-dt-button))

(define-subwidget (spirograph-widget reset-button) (q+:make-qpushbutton "Reset" spirograph-widget)
  "Reset all-offsets to 0")

(define-slot (spirograph-widget type-changed) ()
  "Handle radio button changes that hcange the curve type."
  (declare (connected epitrochoid-button (released)))
  (declare (connected hypotrochoid-button (released)))
  (cond 
    ;; Epitrochoid selected
    ((q+:is-checked epitrochoid-button)
     (setf (slot-value fft-viewer 'x-function) #'epitrochoid-x)
     (setf (slot-value fft-viewer 'y-function) #'epitrochoid-y))

    ;; Hypotrochoid selected
    ((q+:is-checked hypotrochoid-button)
     (setf (slot-value fft-viewer 'x-function) #'hypotrochoid-x)
     (setf (slot-value fft-viewer 'y-function) #'hypotrochoid-y))
    
    ;; One of the above should always be true, but just in case...
    ;; Print a warning and toggle the  epitrochoid-button
    (t
     (format t "Warning: No type selected, defaulting to epitrochoid.~%")
     (q+:set-checked epitrochoid-button t)
     (setf (slot-value fft-viewer 'x-function) #'epitrochoid-x)
     (setf (slot-value fft-viewer 'y-function) #'epitrochoid-y)))

  ;; Repaint to reflect the changes
  (q+:repaint fft-viewer))

(define-slot (spirograph-widget dt-changed) ()
  "Handle radio button changes that hcange the curve type."
  (declare (connected pi-over-dt-button (released)))
  (declare (connected use-dt-button (released)))
  (cond 
    ;; Epitrochoid selected
    ((q+:is-checked use-dt-button)
     (setf (slot-value fft-viewer 'dt-type) :normal))

    ;; Hypotrochoid selected
    ((q+:is-checked pi-over-dt-button)
     (setf (slot-value fft-viewer 'dt-type) :over-pi))
    
    ;; One of the above should always be true, but just in case...
    ;; Print a warning and toggle the  epitrochoid-button
    (t
     (format t "Warning: No dt type selected, defaulting to normal.~%")
     (setf (slot-value fft-viewer 'dt-type) :normal))
     (q+:set-checked use-dt-button t))

  ;; Repaint to reflect the changes
  (q+:repaint fft-viewer))

(define-slot (spirograph-widget steps-changed) ((value int))
  "Handle changes to the steps-spin box."
  (declare (connected steps-spin (value-changed int)))
  (declare (connected a-low-spin (value-changed int)))
  (declare (connected a-high-spin (value-changed int)))
  (declare (connected b-low-spin (value-changed int)))
  (declare (connected b-high-spin (value-changed int)))
  (declare (connected h-low-spin (value-changed int)))
  (declare (connected h-high-spin (value-changed int)))
  (declare (connected dt-1-low-spin (value-changed int)))
  (declare (connected dt-1-high-spin (value-changed int)))
  (declare (connected dt-2-low-spin (value-changed int)))
  (declare (connected dt-2-high-spin (value-changed int)))
  (setf (slot-value fft-viewer 'steps) (q+:value steps-spin))
  (setf (slot-value fft-viewer 'a-low) (q+:value a-low-spin))
  (setf (slot-value fft-viewer 'a-high) (q+:value a-high-spin))
  (setf (slot-value fft-viewer 'b-low) (q+:value b-low-spin))
  (setf (slot-value fft-viewer 'b-high) (q+:value b-high-spin))
  (setf (slot-value fft-viewer 'h-low) (q+:value h-low-spin))
  (setf (slot-value fft-viewer 'h-high) (q+:value h-high-spin))
  (setf (slot-value fft-viewer 'dt-1-low) (q+:value dt-1-low-spin))
  (setf (slot-value fft-viewer 'dt-1-high) (q+:value dt-1-high-spin))
  (setf (slot-value fft-viewer 'dt-2-low) (q+:value dt-2-low-spin))
  (setf (slot-value fft-viewer 'dt-2-high) (q+:value dt-2-high-spin))
  (q+:repaint fft-viewer))

(define-slot (spirograph-widget values-changed) ((value double))
  "Handle changes to all of the spin boxes except steps."
  (declare (connected a-val-spin (value-changed double)))
  (declare (connected b-val-spin (value-changed double)))
  (declare (connected h-val-spin (value-changed double)))
  (declare (connected a-scale-spin (value-changed double)))
  (declare (connected b-scale-spin (value-changed double)))
  (declare (connected h-scale-spin (value-changed double)))
  (declare (connected dt-1-spin (value-changed double)))
  (declare (connected dt-2-spin (value-changed double)))
  (declare (connected dt-1-scale-spin (value-changed double)))
  (declare (connected dt-2-scale-spin (value-changed double)))
  
  (setf (slot-value fft-viewer 'a-val) (q+:value a-val-spin))
  (setf (slot-value fft-viewer 'b-val) (q+:value b-val-spin))
  (setf (slot-value fft-viewer 'h-val) (q+:value h-val-spin))
  (setf (slot-value fft-viewer 'a-scale) (q+:value a-val-spin))
  (setf (slot-value fft-viewer 'b-scale) (q+:value b-val-spin))
  (setf (slot-value fft-viewer 'h-scale) (q+:value h-val-spin))
  (setf (slot-value fft-viewer 'dt-1) (q+:value dt-1-spin))
  (setf (slot-value fft-viewer 'dt-2) (q+:value dt-2-spin))
  (setf (slot-value fft-viewer 'dt-1-scale) (q+:value dt-1-scale-spin))
  (setf (slot-value fft-viewer 'dt-2-scale) (q+:value dt-2-scale-spin))
  (q+:repaint fft-viewer))



(define-subwidget (spirograph-widget control-layout) (q+:make-qvboxlayout spirograph-widget)
  "Layout all of the control widgets in a vertical box layout."

  ;; Create horizontal layouts to hold the labels and spinboxes
  (let (;; (a-inner (q+:make-qhboxlayout))
        ;; (b-inner (q+:make-qhboxlayout))
        ;; (h-inner (q+:make-qhboxlayout))
        (a-layout (q+:make-qhboxlayout))
        (b-layout (q+:make-qhboxlayout))
        (h-layout (q+:make-qhboxlayout))
        (dt-1-layout (q+:make-qhboxlayout))
        (dt-2-layout (q+:make-qhboxlayout))
        (other-layout (q+:make-qhboxlayout)))
    
    ;; Populate the horizontal layouts and add them to the top level vertical layout
    (q+:add-widget a-layout (q+:make-qlabel "A: " spirograph-widget))
    (q+:add-widget a-layout a-val-spin)
    (q+:add-widget a-layout a-scale-spin)
    (q+:add-widget a-layout a-low-spin)
    (q+:add-widget a-layout a-high-spin)

    (q+:add-widget b-layout (q+:make-qlabel "B: " spirograph-widget))
    (q+:add-widget b-layout b-val-spin)
    (q+:add-widget b-layout b-scale-spin)
    (q+:add-widget b-layout b-low-spin)
    (q+:add-widget b-layout b-high-spin)

    (q+:add-widget h-layout (q+:make-qlabel "H: " spirograph-widget))
    (q+:add-widget h-layout h-val-spin)
    (q+:add-widget h-layout h-scale-spin)
    (q+:add-widget h-layout h-low-spin)
    (q+:add-widget h-layout h-high-spin)

    (q+:add-widget dt-1-layout (q+:make-qlabel "dt1: " spirograph-widget))
    (q+:add-widget dt-1-layout dt-1-spin)
    (q+:add-widget dt-1-layout dt-1-scale-spin)
    (q+:add-widget dt-1-layout dt-1-low-spin)
    (q+:add-widget dt-1-layout dt-1-high-spin)

    (q+:add-widget dt-2-layout (q+:make-qlabel "dt2: " spirograph-widget))
    (q+:add-widget dt-2-layout dt-2-spin)
    (q+:add-widget dt-2-layout dt-2-scale-spin)
    (q+:add-widget dt-2-layout dt-2-low-spin)
    (q+:add-widget dt-2-layout dt-2-high-spin)

    (q+:add-widget other-layout (q+:make-qlabel "steps: " spirograph-widget))
    (q+:add-widget other-layout steps-spin)
    (q+:add-widget other-layout use-dt-button)
    (q+:add-widget other-layout pi-over-dt-button)

    ;; Add the radio buttons directly to the vertical layout
    (q+:add-widget other-layout epitrochoid-button)
    (q+:add-widget other-layout hypotrochoid-button)

    (q+:add-widget other-layout reset-button)

    (q+:add-layout control-layout a-layout)
    (q+:add-layout control-layout b-layout)
    (q+:add-layout control-layout h-layout)
    (q+:add-layout control-layout dt-1-layout)
    (q+:add-layout control-layout dt-2-layout)
    (q+:add-layout control-layout other-layout)

    ;; Finally add the spirograph viewer directly to the vertical layout
    (q+:add-widget control-layout fft-viewer)))


(define-signal (spirograph-widget open-mp3) (string))

(define-slot (spirograph-widget reset-pressed) ()
  "Handle the reset button."
  (declare (connected reset-button (released)))
  (setf (slot-value fft-viewer 'a-offset) 0.0)
  (setf (slot-value fft-viewer 'b-offset) 0.0)
  (setf (slot-value fft-viewer 'h-offset) 0.0)
  (setf (slot-value fft-viewer 'dt-1-offset) 0.0)
  (setf (slot-value fft-viewer 'dt-2-offset) 0.0))



(define-slot (spirograph-widget open-mp3) ((file-name string))
  (declare (connected spirograph-widget (open-mp3 string)))
  (let* ((new-mp3-file (read-mp3-file file-name))
         (sduration (mp3-file-duration-in-seconds new-mp3-file))
         (tframes (ceiling (* sduration *fps*))))

    (setf (slot-value fft-viewer 'a-offset) 0.0)
    (setf (slot-value fft-viewer 'b-offset) 0.0)
    (setf (slot-value fft-viewer 'h-offset) 0.0)
    (setf (slot-value fft-viewer 'dt-1-offset) 0.0)
    (setf (slot-value fft-viewer 'dt-2-offset) 0.0)

    (setf (slot-value fft-viewer 'the-mp3) new-mp3-file)
    (setf (slot-value fft-viewer 'song-duration) sduration)
    (setf (slot-value fft-viewer 'start-time) (get-internal-real-time))))


(define-widget main-window (QMainWindow)
  ((mixer :initform (mixalot:create-mixer))
   (current-stream :initform nil)))

(define-override (main-window close-event) (ev)
  (mixalot:mixer-remove-all-streamers mixer)
  (mixalot:destroy-mixer mixer)
  (q+:accept ev))


(define-menu (main-window File)
  (:item ("Open" (ctrl o))
         (open-file main-window))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Interactively view and manipulate FFT data.")))


(define-subwidget (main-window spirograph-viewer) (make-instance 'spirograph-widget)
  "The central spirograph-widget.")


(define-slot (main-window open open-file) ()
  (let ((filename (q+:qfiledialog-get-open-file-name main-window "Select File"
                                                     (q+:qdesktopservices-storage-location 
                                                      (q+:qdesktopservices.music-location))
                                                     "*.mp3")))
    (when (and filename (> (length filename) 0))
      (signal! spirograph-viewer (open-mp3 string) filename)
      (when current-stream (mixalot:mixer-remove-streamer mixer current-stream))
      (setf current-stream (mixalot-mp3:make-mp3-streamer filename))
      (mixalot:mixer-add-streamer mixer current-stream))))

(define-initializer (main-window setup)
  "Set the window title and set the fft-controls to be the central widget."
  (setf (q+:window-title main-window) "Interactive FFT Explorer")
  (setf (q+:central-widget main-window) spirograph-viewer))

(defun main ()
  "Create the main window."
  (trivial-main-thread:call-in-main-thread #'mixalot:main-thread-init)
  (with-main-window (window (make-instance 'main-window))))


