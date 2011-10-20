(defpackage :lapis-engine-ffi
  (:nicknames :lapis-ffi)
  (:use #:cl #:cffi)
  (:export #:init
           #:end
           #:gl-begin
           #:gl-end
           #:set-video-mode
           #:draw-rect
           #:draw-point
           #:enable-smooth-lines
           #:draw-line
           #:prepare-render
           #:flip
           #:get-tick
           #:poll-event
           #:sdl-event
           #:sdl-event-mouse-motionp
           #:sdl-event-mouse-motion
           #:sdl-event-quitp
           #:sdl-event-resizep
           #:sdl-event-resize
           #:sdl-event-keyp
           #:sdl-event-key))

(in-package :lapis-ffi)

(load-foreign-library "libGL.so")
(load-foreign-library "libGLU.so")
(load-foreign-library "libSDL.so")
(load-foreign-library "libSDL_mixer.so" :search-path #p"/usr/local/lib/")
(load-foreign-library "libSDL_ttf.so")
(load-foreign-library "libSDL_image.so")
(load-foreign-library "liblapis.so" :search-path #p".")

(defconstant +gl-points+ #x0)
(defconstant +gl-lines+ #x1)
(defconstant +gl-line-loop+ #x2)
(defconstant +gl-line-strip+ #x3)
(defconstant +gl-triangles+ #x4)
(defconstant +gl-triangle-strip+ #x5)
(defconstant +gl-triangle-fan+ #x6)
(defconstant +gl-quads+ #x7)
(defconstant +gl-quad-strip+ #x8)
(defconstant +gl-polygon+ #x9)

;; TODO: instead of draw-rect, draw-line etc. just make direct calls to glvector, etc.

(defcfun ("glBegin" lsdl-gl-begin) :void
  (type :int))
(defcfun ("glEnd" gl-end) :void)

(defcfun ("lapis_init" init) :int)
(defcfun ("lapis_deinit" end) :void)

(defmacro no-fp-traps (&body body)
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body))

(defcfun ("lsdl_set_video_mode" set-video-mode-f) :void
  (screen-width :uint)
  (screen-height :uint)
  (fullscreen :uchar)
  (resizeable :uchar))

(defun set-video-mode (&rest args)
  (no-fp-traps
   (apply #'set-video-mode-f args)))

(defcfun ("lsdl_fill_rect" lsdl-fill-rect) :void
  (x :float)
  (y :float)
  (w :float)
  (h :float)
  (red :float)
  (green :float)
  (blue :float))

(defcfun ("lsdl_draw_point" lsdl-draw-point) :void
  (x :float)
  (y :float)
  (red :float)
  (green :float)
  (blue :float))

(defcfun ("lsdl_prepare_render" prepare-render) :void)

(defcfun ("lsdl_get_tick" get-tick) :uint)

(defcfun ("lsdl_flip" lsdl-flip) :void)

(defcfun ("lsdl_enable_smooth_lines" lsdl-enable-smooth-lines) :void)
(defcfun ("lsdl_draw_line" lsdl-draw-line) :void
  (sx :float)
  (sy :float)
  (ex :float)
  (ey :float)
  (sr :float)
  (sg :float)
  (sb :float)
  (er :float)
  (eg :float)
  (eb :float))

(defun enable-smooth-lines ()
  (no-fp-traps
    (funcall #'lsdl-enable-smooth-lines)))

(defun draw-line (&rest args)
  (no-fp-traps
    (apply #'lsdl-draw-line (mapcar #'float args))))

(defun draw-point (&rest args)
  (no-fp-traps
    (apply #'lsdl-draw-point args)))

(defun gl-begin (&rest args)
  (no-fp-traps
   (apply #'lsdl-gl-begin args)))

(defun draw-rect (&rest args)
  (no-fp-traps
   (apply #'lsdl-fill-rect args)))

(defun flip ()
  (no-fp-traps
    (funcall #'lsdl-flip)))

;; event stuff

(defcstruct event-data-data-key
  (type :int)
  (mod :int)
  (key :int))

(defcstruct event-data-data-resize
  (width :int)
  (height :int))

(defcstruct event-data-data-mouse-motion
  (x :int)
  (y :int))

(defcstruct event-data-data-mouse-button
  (type :int)
  (x :int)
  (y :int)
  (button :int))

(defcunion event-data-data
  (key (event-data-data-key))
  (resize (event-data-data-resize))
  (mouse-motion (event-data-data-mouse-motion))
  (mouse-button (event-data-data-mouse-button)))

(defcstruct sdl-event
  (type :int)
  (data event-data-data))

(defcfun ("lsdl_poll_event" poll-event) :int
  (event (:pointer sdl-event)))

(defun sdl-event-keyp (event)
  (= (foreign-slot-value event 'sdl-event 'type) 0))

(defun sdl-event-mouse-buttonp (event)
  (= (foreign-slot-value event 'sdl-event 'type) 1))

(defun sdl-event-mouse-motionp (event)
  (= (foreign-slot-value event 'sdl-event 'type) 2))

(defun sdl-event-resizep (event)
  (= (foreign-slot-value event 'sdl-event 'type) 3))

(defun sdl-event-quitp (event)
  (= (foreign-slot-value event 'sdl-event 'type) 4))

(defun sdl-event-key (event)
  (foreign-slot-value
   (foreign-slot-pointer
    (foreign-slot-pointer event 'sdl-event 'data)
    'event-data-data 'key)
   'event-data-data-key 'key))

(defun sdl-event-mouse-motion (event)
  (with-foreign-slots ((x y) 
                       (foreign-slot-pointer
                        (foreign-slot-pointer event 'sdl-event 'data)
                        'event-data-data 'mouse-motion)
                       event-data-data-mouse-motion)
    (values x y)))

(defun sdl-event-resize (event)
  (with-foreign-slots ((width height) 
                       (foreign-slot-pointer
                        (foreign-slot-pointer event 'sdl-event 'data)
                        'event-data-data 'resize)
                       event-data-data-resize)
    (values width height)))