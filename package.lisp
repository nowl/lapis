(defpackage :lapis-engine
  (:nicknames :lapis)
  (:use #:cl 
		#:cffi
        #:lapis-ffi)
  (:export #:init
           #:end
           #:set-video-mode
           #:fill-rect
           #:enable-smooth-lines
           #:draw-line
           #:prepare-render
           #:flip))