(require 'asdf)

(push "/Users/vb/.sbcl/systems/" asdf:*central-registry*)

(asdf:oos 'asdf:load-op 'swank)
(setf swank:*use-dedicated-output-stream* nil)
(swank:create-server :port 4005 :dont-close t)

(setq *clocc-root* "/Users/vb/.sbcl/site/clocc/") ; or whatever ...

(asdf:oos 'asdf:load-op 'sondesh)

(in-package :com.sondesh.database)
