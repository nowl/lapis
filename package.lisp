(defpackage :lapis-engine
  (:nicknames :lapis)
  (:use #:cl 
		#:cffi
        #:lapis-ffi)
  (:export #:init
           #:end
           #:set-video-mode
           #:fill-rect
           #:prepare-render
           #:flip))