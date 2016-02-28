FROM jupyter/notebook:stable
MAINTAINER gheber <gheber@hdfgroup.org>

RUN apt-get update

RUN DEBIAN_FRONTEND=noninteractive apt-get -y install wget unzip

# SBCL

RUN wget -nv http://downloads.sourceforge.net/project/sbcl/sbcl/1.3.2/sbcl-1.3.2-x86-64-linux-binary.tar.bz2 && tar -jxvf sbcl-1.3.2-x86-64-linux-binary.tar.bz2 && cd sbcl-1.3.2-x86-64-linux && sh install.sh

# Quicklisp

RUN wget -nv https://beta.quicklisp.org/quicklisp.lisp && sbcl --load quicklisp.lisp --non-interactive --eval "(quicklisp-quickstart:install)"

RUN echo '#-quicklisp' >> /root/.sbclrc
RUN echo '(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"' >> /root/.sbclrc
RUN echo '                                       (user-homedir-pathname))))' >> /root/.sbclrc
RUN echo '  (when (probe-file quicklisp-init)' >> /root/.sbclrc
RUN echo '    (load quicklisp-init)))' >> /root/.sbclrc
RUN echo '(push #p"/workspace/" asdf:*central-registry*)' >> /root/.sbclrc

# cl-jupyter

RUN wget -nv https://github.com/fredokun/cl-jupyter/archive/master.zip && unzip master.zip && mv cl-jupyter-master /root && python3 /root/cl-jupyter-master/install-cl-jupyter.py && sbcl --load /root/cl-jupyter-master/cl-jupyter.lisp --non-interactive

# hdf5

RUN wget http://www.hdfgroup.org/ftp/HDF5/current/src/hdf5-1.8.16.tar.gz && tar -zxvf hdf5-1.8.16.tar.gz && cd hdf5-1.8.16 && ./configure --prefix=/root --enable-production && make -j2 && make install

# hdf5-cffi

RUN wget https://github.com/HDFGroup/hdf5-cffi/archive/master.zip && unzip master.zip -d /root/quicklisp/local-projects && rm master.zip

RUN mv /root/quicklisp/local-projects/hdf5-cffi-master /root/quicklisp/local-projects/hdf5-cffi 

RUN cd /root/quicklisp/local-projects/hdf5-cffi && CC=/root/bin/h5cc LD_LIBRARY_PATH=/root/lib make && LD_LIBRARY_PATH=/root/lib sbcl --eval "(ql:quickload :hdf5-cffi)"

# alcove
