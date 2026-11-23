/usercode/FILESYSTEM$ bash /usercode/FILESYSTEM/.codesignal/setup.sh -a
Setting up environment…
Collecting flask
  Downloading flask-3.1.3-py3-none-any.whl (103 kB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 103.4/103.4 KB 5.7 MB/s eta 0:00:00
Collecting blinker>=1.9.0
  Downloading blinker-1.9.0-py3-none-any.whl (8.5 kB)
Requirement already satisfied: markupsafe>=2.1.1 in /usr/local/lib/python3.10/dist-packages (from flask) (3.0.4)
Collecting werkzeug>=3.1.0
  Downloading werkzeug-3.1.9-py3-none-any.whl (228 kB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 228.7/228.7 KB 14.4 MB/s eta 0:00:00
Requirement already satisfied: jinja2>=3.1.2 in /usr/local/lib/python3.10/dist-packages (from flask) (3.1.6)
Collecting itsdangerous>=2.2.0
  Downloading itsdangerous-2.2.0-py3-none-any.whl (16 kB)
Collecting click>=8.1.3
  Downloading click-8.5.1-py3-none-any.whl (126 kB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 126.2/126.2 KB 21.0 MB/s eta 0:00:00
Installing collected packages: werkzeug, itsdangerous, click, blinker, flask
  Attempting uninstall: blinker
    Found existing installation: blinker 1.4
    Not uninstalling blinker at /usr/lib/python3/dist-packages, outside environment /usr
    Can't uninstall 'blinker'. No files were found to uninstall.
Successfully installed blinker-1.9.0 click-8.5.1 flask-3.1.3 itsdangerous-2.2.0 werkzeug-3.1.9
Installing package into ‘/usr/local/lib/R/site-library’
(as ‘lib’ is unspecified)
also installing the dependency ‘yardstick’

trying URL 'https://cloud.r-project.org/src/contrib/yardstick_1.5.0.tar.gz'
trying URL 'https://cloud.r-project.org/src/contrib/caret_7.1-0.tar.gz'
* installing *source* package ‘yardstick’ ...
** this is package ‘yardstick’ version ‘1.5.0’
** package ‘yardstick’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘cc (Ubuntu 11.4.0-1ubuntu1~22.04.3) 11.4.0’
cc -I"/usr/share/R/include" -DNDEBUG       -fpic  -g -O2 -ffile-prefix-map=/build/r-base-iLJhrH/r-base-4.6.1=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2  -c init.c -o init.o
cc -I"/usr/share/R/include" -DNDEBUG       -fpic  -g -O2 -ffile-prefix-map=/build/r-base-iLJhrH/r-base-4.6.1=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2  -c mcc-multiclass.c -o mcc-multiclass.o
cc -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -flto=auto -ffat-lto-objects -flto=auto -Wl,-z,relro -o yardstick.so init.o mcc-multiclass.o -L/usr/lib/R/lib -lR
installing to /usr/local/lib/R/site-library/00LOCK-yardstick/00new/yardstick/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (yardstick)
* installing *source* package ‘caret’ ...
** this is package ‘caret’ version ‘7.1-0’
** package ‘caret’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘cc (Ubuntu 11.4.0-1ubuntu1~22.04.3) 11.4.0’
cc -I"/usr/share/R/include" -DNDEBUG       -fpic  -g -O2 -ffile-prefix-map=/build/r-base-iLJhrH/r-base-4.6.1=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2  -c caret.c -o caret.o
cc -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -flto=auto -ffat-lto-objects -flto=auto -Wl,-z,relro -o caret.so caret.o -L/usr/lib/R/lib -lR
installing to /usr/local/lib/R/site-library/00LOCK-caret/00new/caret/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (caret)

The downloaded source packages are in
        ‘/tmp/Rtmpc0Q7Q8/downloaded_packages’
Setup steps complete.
Environment is set and ready to code!
/usercode/FILESYSTEM$ 