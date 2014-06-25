
(defpackage :baktoo
  (:use :common-lisp
:lispbuilder-sdl
:rutils.list)
  (:export :start))

(in-package :baktoo)

(defparameter *grow-prob* 0.3
  "Probability for a cell to spawn a new one when the conditions are right")
(defparameter *lengthen-prob* 0.3
  "Probability for a cell to lengthen (IF it doesn't grow)")

(defparameter *starting-length* 15)

(defparameter *growth-condition* '(10 20)
  "If there are less than the given number of cells in the given radius,
then make a new cell")

(defparameter *alive-condition* '(10 10)
  "The number of cells and the radius (respectively), above which a cell
stops taking any actions.
E.g. no more than 5 other cells within a radius of 10")

(defparameter *dimensions* '(800 600)
  "Width and height of the screen")

(defparameter *baktus-draw-radius* 2)

(defparameter *baktrees* nil
  "All the top-level trees, created by user clicks; each tree is a new
random color")

(defstruct pos
  x y)

(defstruct baktus
  pos color children)

(defstruct child-node
  angle distance baktus)

;; Rough "location" map -- cpu is expensive, but memory is ***in cheap!
(defparameter *locations* nil)
(defparameter *resolution* 5)

(defun add-location (x y)
  (let* ((k (cons (round (/ x *resolution*))
  (round (/ y *resolution*))))
 (l (gethash k *locations*)))
    (setf (gethash k *locations*) (push (cons x y) l))))

(defun remove-location (x y)
  (let* ((k (cons (round (/ x *resolution*))
  (round (/ y *resolution*))))
 (l (gethash k *locations*)))
    (if (= 1 (length l))
(remhash k *locations*)
 (setf (gethash k *locations*) (remove (cons x y) l :test #'equal)))))

(defun find-positions (x y radius)
  (let ((steps (/ radius *resolution*))
 (rx (round (/ x *resolution*)))
(ry (round (/ y *resolution*)))
(positions nil))
    (loop for i from (- 0 steps) to steps
 do (loop for j from (- 0 steps) to steps
 do (let* ((k (cons (+ rx i) (+ ry j)))
   (l (gethash k *locations*)))
      (unless (null l)
(setf positions (append positions l))))))
    positions))


(defun process-all-trees (per-node-func per-child-node-func)
  (dolist (elem *baktrees*)
    (funcall #'process-tree elem per-node-func per-child-node-func)))

(defun process-tree (root per-node-func per-child-node-func)
  "Recursively process all nodes of a given tree"
  (funcall per-node-func root)
  (dolist (elem (baktus-children root))
    (process-tree (child-node-baktus elem) per-node-func
per-child-node-func)
    (funcall per-child-node-func root elem)))

(defun draw-baktus (root)
  "Requires positions to be precomputed"
  (sdl:draw-filled-circle-* (round (pos-x (baktus-pos root)))
    (round (pos-y (baktus-pos root)))
    *baktus-draw-radius*
    :color (baktus-color root)))

(defun draw-baktus-connection (root child)
  (sdl:draw-line-* (round (pos-x (baktus-pos root)))
   (round (pos-y (baktus-pos root)))
   (round (pos-x (baktus-pos (child-node-baktus child))))
   (round (pos-y (baktus-pos (child-node-baktus child))))
   :color (baktus-color root)))

(defun get-child-pos (angle distance root-pos)
  (make-pos :x (+ (pos-x root-pos) (* distance (cos angle)))
    :y (+ (pos-y root-pos) (* distance (sin angle)))))

;; TODO(krato): Rotate the children in a nicer manner
(defun get-new-baktus (root)
  "Insert a new child, re-arranging the other existing children if
necessary"
  (let* ((num-children (1+ (length (baktus-children root))))
 (angle-per-child (/ (* 2 pi) num-children))
 (angle-for-new-child (* (1- num-children) angle-per-child))
 (angle-perturbation (random (* 2 pi)))
 (new-pos (get-child-pos (+ angle-perturbation angle-for-new-child)
 *starting-length*
 (baktus-pos root)))
 (newchildren (mapindex #'(lambda (index child)
    (make-child-node :angle (+ angle-perturbation (* index angle-per-child))
     :distance (child-node-distance child)
     :baktus (make-baktus :pos (get-child-pos (+ angle-perturbation (*
index angle-per-child))
      (child-node-distance child)
      (baktus-pos root))
  :color (baktus-color (child-node-baktus child))
  :children (baktus-children (child-node-baktus child)))))
 (baktus-children root))))
    ;; TODO(krato): Replace old positions with new ones
    (setf (baktus-children root) newchildren)
    (add-location (pos-x new-pos) (pos-y new-pos))
    (make-child-node :angle (+ angle-perturbation angle-for-new-child)
     :distance *starting-length*
     :baktus (make-baktus :pos new-pos
  :color (baktus-color root)
  :children nil))))

(defun grow-baktus (root node-distances)
  (when (and (can-grow node-distances)
     (> (random 1.0) *grow-prob*))
    (push (get-new-baktus root) (baktus-children root))))

(defun min-close-nodes (distances criteria &optional (close-nodes 0))
  "Helper for 'can-grow' and 'is-alive' below"
  (if (null distances)
      t
      (if (> close-nodes criteria)
  nil
  (min-close-nodes (cdr distances) criteria (1+ close-nodes)))))

(defun can-grow (distances)
  "The first distance (with itself) is 0, so skip that. Check if the
first non-zero distance is greater than the minimum"
  (min-close-nodes distances (first *growth-condition*)))

(defun is-alive (distances)
  "Check distances in ascending order, as long as they are less than
  the 'alive-condition' distance. If more than the configured number
  of cells are found nearby, the current cell is not considered
  alive."
  (min-close-nodes distances (first *alive-condition*)))

(defun possibly-extend-child (root child)
  (declare (ignore root child))
  )

(defun get-baktus-positions ()
  (let ((positions nil))
    (process-all-trees #'(lambda (node)
   (push (baktus-pos node) positions))
       #'(lambda (root child) (declare (ignore root child))))
    positions))

(defun node-distance (pos1 pos2)
  (distance pos1 (pos-x pos2) (pos-y pos2)))

(defun distance (pos x y)
    (sqrt (+ (expt (- (pos-x pos) x) 2)
     (expt (- (pos-y pos) y) 2))))

(defun get-closest-distances (node neighbors)
  (mapcar #'(lambda (pos)
      (distance (baktus-pos node)
(car pos) (cdr pos)))
  neighbors))

(defun grow-nodes ()
  "For each node, if it is alive, see if we want to spawn a new cell nearby.
Then, process all its children. If a child didn't grow, give it a chance to
move away"
  (process-all-trees #'(lambda (node)
 (let* ((neighbors (find-positions (pos-x (baktus-pos node))
   (pos-y (baktus-pos node))
   (max (second *growth-condition*)
(second *alive-condition*))))
(node-distances (get-closest-distances node neighbors)))
   (when (is-alive node-distances)
     (grow-baktus node node-distances))))
     #'possibly-extend-child))

(defun draw-nodes ()
  (process-all-trees #'draw-baktus #'draw-baktus-connection))

(defun init-state ()
  (setf *baktrees* nil)
  (setf *locations* (make-hash-table :test #'equal)))

(let* ((predefined-colors (list sdl::*white*
sdl::*red*
sdl::*blue*
sdl::*yellow*
 sdl::*green*
sdl::*magenta*
sdl::*cyan*))
       (color-count 0))
  (defun make-new-baktus (x y)
    (let ((new-color (nth color-count predefined-colors)))
      (setf color-count (mod (1+ color-count) (length predefined-colors)))
      (add-location x y)
      (push (make-baktus :pos (make-pos :x x :y y)
 :color new-color
 :children nil)
    *baktrees*))))

;; Helpful for debugging
(defun count-nodes ()
  (let ((c 0))
    (process-all-trees #'(lambda (n) (declare (ignore n)) (incf c))
       #'(lambda (r c) (declare (ignore r c))))
    c))

(defun start ()
  (init-state)
  (sdl:with-init ()
    (sdl:window (first *dimensions*) (second *dimensions*)
:title-caption "Baktoo"
:icon-caption "Baktoo")
    (sdl:with-events ()
            (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:key-down-event (:key key)
       (when (sdl:key= key :sdl-key-escape)
 (sdl:push-quit-event)))
      (:mouse-button-down-event (:x x :y y)
(make-new-baktus x y))
      (:idle ()
     (time (progn
       (sdl:clear-display sdl:*black*)
       (grow-nodes)
       (draw-nodes)
       (sleep 0.2)
       (sdl:update-display)))))))
