(defpackage :lapis
  (:use #:cl 
		#:cffi)
  (:export #:init
           #:set-video-mode
           #:fill-rect
           #:prepare-render
           #:flip
           #:set-max-frame-skip
           #:set-ticks-per-second
           #:mainloop))