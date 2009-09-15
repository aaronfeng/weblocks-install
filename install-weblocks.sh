# Creates a self contained weblocks project with all the dependencies frozen within the project
# by Aaron Feng
# aaron.feng@gmail.com
# http://twitter.com/aaronfeng

set -e
set -x

if [ -z $1 ]
then
  echo "You must supply a project name "
  echo ""
  exit 1
fi

source packages.sh

if [ -z "$PACKAGES" ]; then
  echo "\$PACKAGES not set: You must specify packages to install with weblocks. "
  echo ""
  exit 1
fi

if [ -z "$PACKAGES_URL" ]; then
  echo "\$PACKAGES_URL not set: You must specify url to download the packages. "
  echo ""
  exit 1
fi

set -u

PROJECT_PATH=`dirname $1`/`basename $1`
PROJECT_NAME=`basename $1`
LIB=$PROJECT_PATH/lib
LIB_SYSTEMS=$LIB/systems
LIB_SRC=$LIB/src
SRC=$PROJECT_PATH/src
SCRIPT=$PROJECT_PATH/script
CONF=$PROJECT_PATH/conf
DATA=$PROJECT_PATH/data
WEBLOCKS_PUB=$LIB_SRC/weblocks-stable/pub

if ! hash sbcl 2>/dev/null 
then
  echo -e "\033[1m Unable to find sbcl on the path.  Exiting. \033[0m"
  exit 1
fi

test -d $PROJECT_PATH      || mkdir $PROJECT_PATH
test -d $LIB               || mkdir $LIB
test -d $LIB_SYSTEMS       || mkdir $LIB_SYSTEMS
test -d $LIB_SRC           || mkdir $LIB_SRC
test -d $SRC               || mkdir $SRC
test -d $SCRIPT            || mkdir $SCRIPT
test -d $CONF              || mkdir $CONF
test -d $DATA              || mkdir $DATA

INIT_SESSION="$SRC/init-session.lisp"
if [ ! -e "$INIT_SESSION" ]
then
  cat > "$INIT_SESSION" <<EOF
(in-package :$PROJECT_NAME)

;; Define callback function to initialize new sessions
(defun init-user-session (comp)
  (setf (composite-widgets comp)
  (list (lambda (&rest args)
    (with-html
      (:strong "Happy Hacking!"))))))
EOF
fi

STORES="$CONF/stores.lisp"
if [ ! -e "$STORES" ]
then
  cat > "$STORES" <<EOF
(in-package :$PROJECT_NAME)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *$PROJECT_NAME-store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
       (asdf-system-directory :$PROJECT_NAME)))
EOF
fi

PROJECT_LISP="$PROJECT_PATH/$PROJECT_NAME.lisp"
if [ ! -e "$PROJECT_LISP" ]
then
  cat > "$PROJECT_LISP" <<EOF
(defpackage #:$PROJECT_NAME
  (:use :cl :weblocks
        :f-underscore :anaphora)
  (:import-from :hunchentoot #:header-in
    #:set-cookie #:set-cookie* #:cookie-in
    #:user-agent #:referer)
  (:documentation
   "A web application based on Weblocks."))

(in-package :$PROJECT_NAME)

(export '(start-$PROJECT_NAME stop-$PROJECT_NAME))

;; A macro that generates a class or this webapp

(defwebapp $PROJECT_NAME
    :prefix "/" 
    :description "$PROJECT_NAME: A new application"
    :init-user-session '$PROJECT_NAME::init-user-session
    :autostart nil                   ;; have to start the app manually
    :ignore-default-dependencies nil ;; accept the defaults
    :debug t
    )   

;; Top level start & stop scripts

(defun start-$PROJECT_NAME (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate arguments."
  (apply #'start-weblocks args)
  (start-webapp '$PROJECT_NAME))

(defun stop-$PROJECT_NAME ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp '$PROJECT_NAME)
  (stop-weblocks))
EOF
fi

ASD="$PROJECT_PATH/$PROJECT_NAME.asd"
if [ ! -e "$ASD" ] 
then
  cat > "$ASD" <<EOF
(defpackage #:$PROJECT_NAME-asd
  (:use :cl :asdf))

(in-package :$PROJECT_NAME-asd)

(defsystem $PROJECT_NAME
    :name "$PROJECT_NAME"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "$PROJECT_NAME"
    :depends-on (:weblocks)
    :components ((:file "$PROJECT_NAME")
     (:module conf
      :components ((:file "stores"))
      :depends-on ("$PROJECT_NAME"))
     (:module src 
      :components ((:file "init-session"))
      :depends-on ("$PROJECT_NAME" conf))))
EOF
fi

  
SBCLRC="$PROJECT_PATH/$PROJECT_NAME.sbclrc"
if [ ! -e "$SBCLRC" ]
then
  cat > "$SBCLRC" <<EOF
(require 'asdf)
(require 'sb-posix)
(require :sb-aclrepl)

(defmacro loadsys (sys)
  \`(asdf:oos 'asdf:load-op (quote ,sys)))

(defvar *project-root* (pathname (nth 1 *posix-argv*)))
(defvar *project-lib-systems* (merge-pathnames #p"lib/systems/" *project-root*))

(push *project-root* asdf:*central-registry*)
(push *project-lib-systems* asdf:*central-registry*)

(asdf:operate 'asdf:load-op 'weblocks)

(loadsys :swank)
(swank:create-server :dont-close t)

(loadsys :$PROJECT_NAME)
($PROJECT_NAME:start-$PROJECT_NAME :port 4444)

(if (member "--no-linedit" sb-ext:*posix-argv* :test 'equal)
    (setf sb-ext:*posix-argv* 
    (remove "--no-linedit" sb-ext:*posix-argv* :test 'equal))
    (when (interactive-stream-p *terminal-io*)
      (require :sb-aclrepl)
      (require :linedit)
      (funcall (intern "INSTALL-REPL" :linedit) :wrap-current t)))
EOF
fi

RUN_SCRIPT="$SCRIPT/server"
if [ ! -e "$RUN_SCRIPT" ]
then
  cat > "$RUN_SCRIPT" <<EOF
PROJECT_ROOT=\`dirname \$0\`/../
echo "Project root: \$PROJECT_ROOT"
echo "DELETING old $PROJECT_NAME fasl"
find \$PROJECT_ROOT/src  -iname \*.fasl -delete
sbcl --userinit \$PROJECT_ROOT/$PROJECT_NAME.sbclrc \$PROJECT_ROOT
EOF
  chmod 744 "$RUN_SCRIPT"
fi

for PACKAGE in $PACKAGES; do
  if [ ! -f "$LIB_SRC/$PACKAGE" ] 
  then
    echo -e "\033[33m Downloading $PACKAGE \033[0m"
    curl -C - -o "$LIB_SRC/$PACKAGE" -L "$PACKAGES_URL/$PACKAGE"
    echo -e "\033[32m Sucessfully downloaded $PACKAGE \033[0m"
    tar -xvf "$LIB_SRC/$PACKAGE" -C "$LIB_SRC"
  else
    echo -e "\033[31m $PACKAGE already exists.  Skipping. \033[0m"
  fi
done

if [ ! -d "$PROJECT_PATH/pub" ] 
then
cp -r "$WEBLOCKS_PUB" "$PROJECT_PATH"
fi

cd "$LIB_SYSTEMS"
ln -sf ../src/*/*.asd .
rm weblocks-demo.asd
rm weblocks-test.asd
cd -
