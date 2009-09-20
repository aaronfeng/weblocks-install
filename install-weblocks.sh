# Creates a self contained weblocks project with all the dependencies frozen within the project
# by Aaron Feng
# aaron.feng@gmail.com
# http://twitter.com/aaronfeng

set -e

# enable for debugging
# set -x

if [ -z "$1" ]
then
  echo -e "\033[31m Usage: $0 project-name stable-or-tip \033[0m"
  echo ""
  exit 1
fi

PROJECT_PATH="`dirname $1`/`basename $1`"
PROJECT_NAME="`basename "$1"`"
LIB=$PROJECT_PATH/lib
LIB_SYSTEMS=$LIB/systems
LIB_SRC=$LIB/src
SRC=$PROJECT_PATH/src
SCRIPT=$PROJECT_PATH/script
CONF=$PROJECT_PATH/conf
DATA=$PROJECT_PATH/data
INSTALL_SCRIPT="`dirname "$0"`"

WEBLOCKS_PACKAGE_URL="http://cloud.github.com/downloads/aaronfeng/weblocks-install"
WEBLOCKS_PACKAGE="weblocks-0.8.3.tar.gz"
WEBLOCKS_PUB=$LIB_SRC/weblocks-stable/pub

# for some reason -n doesn't work on Debian, so ! -z is used instead
if [ ! -z "$2" ] && [ "$2" == "tip" ]; then
  echo "\033[33m Using the latest weblocks-dev \033[0m"
  WEBLOCKS_PACKAGE_URL="http://bitbucket.org/S11001001/weblocks-dev/get"
  WEBLOCKS_PACKAGE="tip.tar.gz"
  WEBLOCKS_PUB=$LIB_SRC/weblocks-dev/pub
fi

source "$INSTALL_SCRIPT/packages.sh"

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

if ! hash curl 2>/dev/null 
then
  echo -e "\033[1m Unable to find curl on the path.  Exiting. \033[0m"
  exit 1
else
  echo -e "\033[32m Found curl on the path. \033[0m"
fi

if ! hash sbcl 2>/dev/null 
then
  echo -e "\033[1m Unable to find sbcl on the path.  Exiting. \033[0m"
  exit 1
else
  echo -e "\033[32m Found sbcl on the path. \033[0m"
fi

if [ -z $(echo "(null (member :sb-thread *features*))" | sbcl | grep NIL) ]
then
  echo "SBCL thread is not enabled.  Please compile SBCL thread support."
  exit 1
else
  echo -e "\033[32m sbcl thread is enabled. \033[0m"
fi

echo ""

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
  echo -e "\033[32m Created $INIT_SESSION \033[0m"
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
  echo -e "\033[32m Created $STORES \033[0m"
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
  echo -e "\033[32m Created $PROJECT_LISP \033[0m"
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
  echo -e "\033[32m Created $ASD \033[0m"
fi

  
SBCLRC="$PROJECT_PATH/$PROJECT_NAME.sbclrc"
if [ ! -e "$SBCLRC" ]
then
  cat > "$SBCLRC" <<EOF
(require 'asdf)
(require 'sb-posix)
(require :sb-aclrepl)

(defvar *project-root* (pathname (nth 1 *posix-argv*)))
(defvar *port* (parse-integer (nth 2 *posix-argv*)))
(defvar *swank-port* (parse-integer (nth 3 *posix-argv*)))
(defvar *project-lib-systems* (merge-pathnames #p"lib/systems/" *project-root*))

(push *project-root* asdf:*central-registry*)
(push *project-lib-systems* asdf:*central-registry*)

(if (member "--no-linedit" sb-ext:*posix-argv* :test 'equal)
    (setf sb-ext:*posix-argv* (remove "--no-linedit" sb-ext:*posix-argv* :test 'equal))
    (when (interactive-stream-p *terminal-io*)
      (require :sb-aclrepl)
      (require :linedit)
      (funcall (intern "INSTALL-REPL" :linedit) :wrap-current t)))

(asdf:operate 'asdf:load-op 'weblocks)

(asdf:oos 'asdf:load-op 'swank)
(swank:create-server :dont-close t :port *swank-port*)

(asdf:oos 'asdf:load-op '$PROJECT_NAME)
($PROJECT_NAME:start-$PROJECT_NAME :port *port*)

;; sending "\033[2J" to clear screen
(format t "~C[2J~%" #\Esc)

(format t "Welcome to weblocks~%")
(format t "Weblocks is running on port ~S~%" *port*)
(format t "Swank is running on port ~S~%" *swank-port*)
(format t "Use (quit) to exit REPL")
(in-package $PROJECT_NAME)

(defmacro loadsys (sys)
  \`(asdf:oos 'asdf:load-op (quote ,sys)))

(defun quit () (sb-ext:quit))
EOF
  echo -e "\033[32m Created $SBCLRC \033[0m"
fi

RUN_SCRIPT="$SCRIPT/server"
if [ ! -e "$RUN_SCRIPT" ]
then
  cat > "$RUN_SCRIPT" <<EOF
PROJECT_ROOT=\`dirname \$0\`/../
SWANK_PORT=4005
WEBLOCKS_PORT=5555
echo "Project root: \$PROJECT_ROOT"
echo "DELETING old $PROJECT_NAME fasl"
find \$PROJECT_ROOT/src  -iname \*.fasl -delete
sbcl --userinit \$PROJECT_ROOT/$PROJECT_NAME.sbclrc \$PROJECT_ROOT \$WEBLOCKS_PORT \$SWANK_PORT
EOF
  chmod 744 "$RUN_SCRIPT"
  echo -e "\033[32m Created $RUN_SCRIPT \033[0m"
fi


echo ""

if [ ! -f "$LIB_SRC/$WEBLOCKS_PACKAGE" ] 
then
  echo -e "\033[33m Downloading $WEBLOCKS_PACKAGE \033[0m"
  curl -C - -o "$LIB_SRC/$WEBLOCKS_PACKAGE" -L "$WEBLOCKS_PACKAGE_URL/$WEBLOCKS_PACKAGE" > /dev/null 2>&1
  echo -e "\033[33m Unpacking $WEBLOCKS_PACKAGE \033[0m"
  tar -xvf "$LIB_SRC/$WEBLOCKS_PACKAGE" -C "$LIB_SRC" > /dev/null
else
  echo -e "\033[31m Skipping: $WEBLOCKS_PACKAGE already exists. \033[0m"
fi

for PACKAGE in $PACKAGES; do
  if [ ! -f "$LIB_SRC/$PACKAGE" ] 
  then
    echo -e "\033[33m Downloading $PACKAGE \033[0m"
    curl -C - -o "$LIB_SRC/$PACKAGE" -L "$PACKAGES_URL/$PACKAGE" > /dev/null 2>&1
    
    echo -e "\033[33m Unpacking $PACKAGE \033[0m"
    tar -xvf "$LIB_SRC/$PACKAGE" -C "$LIB_SRC" > /dev/null
    echo ""
  else
    echo -e "\033[31m Skipping: $PACKAGE already exists. \033[0m"
  fi
done

if [ ! -d "$PROJECT_PATH/pub" ] 
then
  cp -r "$WEBLOCKS_PUB" "$PROJECT_PATH"
fi

echo ""
echo -e "\033[33m Linking $LIB_SYSTEMS asd files \033[0m"

cd "$LIB_SYSTEMS"
ln -sf ../src/*/*.asd .
rm weblocks-demo.asd
rm weblocks-test.asd
cd - > /dev/null

echo ""
echo -e "\033[32m Sucessfully created $PROJECT_NAME project.\033[0m"
echo -e "\033[32m If anything failed during the installation, it's safe to rerun this script on the same project again.\033[0m"
echo ""
echo -e "\033[32m Execute script/server in $PROJECT_NAME to compile and start up Weblocks.\033[0m"
echo -e "\033[32m The $PROJECT_NAME project will be running on port 5555 and Swank on 4005 by default.\033[0m"
echo -e "\033[32m Please update script/server to run on alternate ports.\033[0m"
echo -e "\033[32m Happy Hacking! \033[0m"
