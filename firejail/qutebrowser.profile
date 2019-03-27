include /nix/store/h41anrmn6ffk7myqyr1v3hqwv65cdhpm-firejail-0.9.54/etc/firejail/globals.local
include /nix/store/h41anrmn6ffk7myqyr1v3hqwv65cdhpm-firejail-0.9.54/etc/firejail/qutebrowser.local

noblacklist ${HOME}/.config/qutebrowser
noblacklist ${HOME}/.cache/qutebrowser
noblacklist ${HOME}/.local/share/qutebrowser

include /nix/store/h41anrmn6ffk7myqyr1v3hqwv65cdhpm-firejail-0.9.54/etc/firejail/disable-common.inc
include /nix/store/h41anrmn6ffk7myqyr1v3hqwv65cdhpm-firejail-0.9.54/etc/firejail/disable-devel.inc
include /nix/store/h41anrmn6ffk7myqyr1v3hqwv65cdhpm-firejail-0.9.54/etc/firejail/disable-programs.inc
#include /nix/store/h41anrmn6ffk7myqyr1v3hqwv65cdhpm-firejail-0.9.54/etc/firejail/disable-interpreters.inc

# Allow python (blacklisted by disable-interpreters.inc)
#noblacklist ${PATH}/python2*
#noblacklist ${PATH}/python3*
#noblacklist /usr/lib/python2*
#noblacklist /usr/lib/python3*
#noblacklist /usr/lib/llvm*


#mkdir ${HOME}/.cache/qutebrowser
#mkdir ${HOME}/.config/qutebrowser
#mkdir ${HOME}/.local/share/qutebrowser
#whitelist ${DOWNLOADS}
#whitelist ${HOME}/.cache/qutebrowser
#whitelist ${HOME}/.config/qutebrowser
#whitelist ${HOME}/.local/share/qutebrowser
#include /nix/store/h41anrmn6ffk7myqyr1v3hqwv65cdhpm-firejail-0.9.54/etc/firejail/whitelist-common.inc

caps.drop all
netfilter
#nodvd
nonewprivs
noroot
#notv
protocol unix,inet,inet6,netlink
seccomp
tracelog

mkdir ${HOME}/.config/qutebrowser
mkdir ${HOME}/.cache/qutebrowser
mkdir ${HOME}/.local/share/qutebrowser

#include /etc/firejail/whitelist-common.inc
# blacklisting of chroot system calls breaks qt webengine
#seccomp.drop @clock,@cpu-emulation,@debug,@module,@obsolete,@raw-io,@reboot,@resources,@swap,acct,add_key,bpf,fanotify_init,io_cancel,io_destroy,io_getevents,io_setup,io_submit,ioprio_set,kcmp,keyctl,mount,name_to_handle_at,nfsservctl,ni_syscall,open_by_handle_at,personality,pivot_root,process_vm_readv,ptrace,remap_file_pages,request_key,setdomainname,sethostname,syslog,umount,umount2,userfaultfd,vhangup,vmsplice
