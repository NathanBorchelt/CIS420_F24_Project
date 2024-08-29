Welcome to the Cluster Design Tools!

This is the directory with software source files. Unless you want to change the software to suit your needs, you will not need any of these source files. (Note that for adding new equipment to databases you are not required to change the source files for the software, just edit the XML files instead).

If you don't need the source files, you can safely delete this directory ("src") altogether, and use only binaries in "bin".

If you still decide to make changes to source code and compile from source, here is how to do it:
1. Enter directory "src"
2. Directory "src/published" will be created automatically after the steps below and will include both sources and binaries
2. To compile binaries for Windows, use "make.bat".
3. Then, copy this entire directory "src" to a GNU/Linux system and run "unix.sh"; this will compile binaries for GNU/Linux and create a ZIP archive, with the version number taken from "version.txt". This archive will contain both the binaries (for Windows and GNU/Linux) and the sources of the software.

The Cluster Design Tools were created by Konstantin S. Solnushkin.

To learn more, please visit: http://ClusterDesign.org
