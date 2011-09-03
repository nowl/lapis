(in-package :lapis)

(load-foreign-library "libGL.so")
(load-foreign-library "libGLU.so")
(load-foreign-library "libSDL.so")
(load-foreign-library "libSDL_mixer.so" :search-path #p"/usr/local/lib/")
(load-foreign-library "libSDL_ttf.so")
(load-foreign-library "libSDL_image.so")
(load-foreign-library "liblapis.so" :search-path #p".")

(defcfun ("lapis_init" init) :int)

(defcfun ("lsdl_set_video_mode" set-video-mode) :void
  (screen-width :uint)
  (screen-height :uint)
  (fullscreen :uchar)
  (resizeable :uchar))

(defcfun ("lsdl_fill_rect" sdl-fill-rect) :void
  (engine :pointer)
  (x :float)
  (y :float)
  (w :float)
  (h :float)
  (red :float)
  (green :float)
  (blue :float))

(defcfun ("lsdl_prepare_render" prepare-render) :void)

(defcfun ("lsdl_get_tick" get-tick) :uint)

(defcfun ("lsdl_flip" sdl-flip) :void
  (engine :pointer))

(defun fill-rect (&rest args)
  (apply #'sdl-fill-rect (null-pointer) args))

(defun flip ()
  (sdl-flip (null-pointer)))

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