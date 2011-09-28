(defpackage :lapis-engine
  (:nicknames :lapis)
  (:use #:cl 
		#:cffi
        #:lapis-ffi)
  (:export #:init
           #:set-video-mode
           #:fill-rect
           #:prepare-render
           #:flip
           #:set-max-frame-skip
           #:set-ticks-per-second
           #:mainloop))