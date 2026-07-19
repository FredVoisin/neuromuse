#|
  This file is a part of neuromuse project.
|#

(defsystem "neuromuse"
  :version "2.12.0"
  :author "Fred Voisin"
  :license " GNU GENERAL PUBLIC LICENSE V3"
  :depends-on (:sb-bsd-sockets)
  :components ((:module "src"
                :components
                ((:file "neuromuse")
		 (:file	"neuromuse-main")
		 (:file "maths&misc")
		 (:file	"mlp")
		 (:file "som")
		 (:file "rosom")
		 (:file "udp"))))
  :description "Neural nets for experiments in music prod by Fred Voisin, since 1999."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "neuromuse-test"))))

(defsystem "neuromuse-test"
  :depends-on (:neuromuse :prove)
  :components ((:module "tests"
                :components
                ((:file "neuromuse"))))
  :description "Test suite for neuromuse.")
