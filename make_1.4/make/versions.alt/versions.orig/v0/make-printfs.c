#define INCLUDEDIR "/usr/local/include"  /* modification by J.Ruthruff, 8/16 */
#define LIBDIR "/usr/local/lib"  /* modification by J.Ruthruff, 8/16 */
#define HAVE_CONFIG_H  /* modification by J.Ruthruff, 8/16 */
int case_coverage = 1;
#undef stderr
#define stderr stdout

/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         commands.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Command processing for GNU Make.
Copyright (C) 1988, 89, 91, 92, 93, 94, 95, 96 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
#include "dep.h"
#include "filedef.h"
#include "variable.h"
#include "job.h"
#include "commands.h"

#undef stderr
#define stderr stdout

extern int remote_kill PARAMS ((int id, int sig));

#ifndef	HAVE_UNISTD_H
extern int getpid ();
#endif

/* Set FILE's automatic variables up.  */

static void
set_file_variables (file)
     register struct file *file;
{
  register char *p;
  char *at, *percent, *star, *less;

#ifndef	NO_ARCHIVES
  /* If the target is an archive member `lib(member)',
     then $@ is `lib' and $% is `member'.  */

  if (ar_name (file->name))
    {
      unsigned int len;
      p = index (file->name, '(');
      at = (char *) alloca (p - file->name + 1);
      bcopy (file->name, at, p - file->name);
      at[p - file->name] = '\0';
      len = strlen (p + 1);
      percent = (char *) alloca (len);
      bcopy (p + 1, percent, len - 1);
      percent[len - 1] = '\0';
    }
  else
#endif	/* NO_ARCHIVES.  */
    {
      at = file->name;
      percent = "";
    }

  /* $* is the stem from an implicit or static pattern rule.  */
  if (file->stem == 0)
    {
      /* In Unix make, $* is set to the target name with
	 any suffix in the .SUFFIXES list stripped off for
	 explicit rules.  We store this in the `stem' member.  */
      register struct dep *d;
      char *name;
      unsigned int len;

#ifndef	NO_ARCHIVES
      if (ar_name (file->name))
	{
	  name = index (file->name, '(') + 1;
	  len = strlen (name) - 1;
	}
      else
#endif
	{
	  name = file->name;
	  len = strlen (name);
	}

      for (d = enter_file (".SUFFIXES")->deps; d != 0; d = d->next)
	{
	  unsigned int slen = strlen (dep_name (d));
	  if (len > slen && !strncmp (dep_name (d), name + (len - slen), slen))
	    {
	      file->stem = savestring (name, len - slen);
	      break;
	    }
	}
      if (d == 0)
	file->stem = "";
    }
  star = file->stem;

  /* $< is the first dependency.  */
  less = file->deps != 0 ? dep_name (file->deps) : "";

  if (file->cmds == default_file->cmds)
    /* This file got its commands from .DEFAULT.
       In this case $< is the same as $@.  */
    less = at;

#define	DEFINE_VARIABLE(name, len, value) \
  (void) define_variable_for_file (name, len, value, o_automatic, 0, file)

  /* Define the variables.  */

  DEFINE_VARIABLE ("<", 1, less);
  DEFINE_VARIABLE ("*", 1, star);
  DEFINE_VARIABLE ("@", 1, at);
  DEFINE_VARIABLE ("%", 1, percent);

  /* Compute the values for $^, $+, and $?.  */

  {
    register unsigned int qmark_len, plus_len;
    char *caret_value, *plus_value;
    register char *cp;
    char *qmark_value;
    register char *qp;
    register struct dep *d;
    unsigned int len;

    /* Compute first the value for $+, which is supposed to contain
       duplicate dependencies as they were listed in the makefile.  */

    plus_len = 0;
    for (d = file->deps; d != 0; d = d->next)
      plus_len += strlen (dep_name (d)) + 1;

    len = plus_len == 0 ? 1 : plus_len;
    cp = plus_value = (char *) alloca (len);

    qmark_len = plus_len;	/* Will be this or less.  */
    for (d = file->deps; d != 0; d = d->next)
      {
	char *c = dep_name (d);

#ifndef	NO_ARCHIVES
	if (ar_name (c))
	  {
	    c = index (c, '(') + 1;
	    len = strlen (c) - 1;
	  }
	else
#endif
	  len = strlen (c);

	bcopy (c, cp, len);
	cp += len;
#if VMS
        *cp++ = ',';
#else
	*cp++ = ' ';
#endif
	if (! d->changed)
	  qmark_len -= len + 1;	/* Don't space in $? for this one.  */
      }

    /* Kill the last space and define the variable.  */

    cp[cp > plus_value ? -1 : 0] = '\0';
    DEFINE_VARIABLE ("+", 1, plus_value);

    /* Make sure that no dependencies are repeated.  This does not
       really matter for the purpose of updating targets, but it
       might make some names be listed twice for $^ and $?.  */

    uniquize_deps (file->deps);

    /* Compute the values for $^ and $?.  */

    cp = caret_value = plus_value; /* Reuse the buffer; it's big enough.  */
    len = qmark_len == 0 ? 1 : qmark_len;
    qp = qmark_value = (char *) alloca (len);

    for (d = file->deps; d != 0; d = d->next)
      {
	char *c = dep_name (d);

#ifndef	NO_ARCHIVES
	if (ar_name (c))
	  {
	    c = index (c, '(') + 1;
	    len = strlen (c) - 1;
	  }
	else
#endif
	  len = strlen (c);

	bcopy (c, cp, len);
	cp += len;
#if VMS
	*cp++ = ',';
#else
	*cp++ = ' ';
#endif
	if (d->changed)
	  {
	    bcopy (c, qp, len);
	    qp += len;
#if VMS
	    *qp++ = ',';
#else
	    *qp++ = ' ';
#endif
	  }
      }

    /* Kill the last spaces and define the variables.  */

    cp[cp > caret_value ? -1 : 0] = '\0';
    DEFINE_VARIABLE ("^", 1, caret_value);

    qp[qp > qmark_value ? -1 : 0] = '\0';
    DEFINE_VARIABLE ("?", 1, qmark_value);
  }

#undef	DEFINE_VARIABLE
}

/* Chop CMDS up into individual command lines if necessary.
   Also set the `lines_flag' and `any_recurse' members.  */

void
chop_commands (cmds)
     register struct commands *cmds;
{
  if (cmds != 0 && cmds->command_lines == 0)
    {
      /* Chop CMDS->commands up into lines in CMDS->command_lines.
	 Also set the corresponding CMDS->lines_flags elements,
	 and the CMDS->any_recurse flag.  */
      register char *p;
      unsigned int nlines, idx;
      char **lines;

      nlines = 5;
      lines = (char **) xmalloc (5 * sizeof (char *));
      idx = 0;
      p = cmds->commands;
      while (*p != '\0')
	{
	  char *end = p;
	find_end:;
	  end = index (end, '\n');
	  if (end == 0)
	    end = p + strlen (p);
	  else if (end > p && end[-1] == '\\')
	    {
	      int backslash = 1;
	      register char *b;
	      for (b = end - 2; b >= p && *b == '\\'; --b)
		backslash = !backslash;
	      if (backslash)
		{
		  ++end;
		  goto find_end;
		}
	    }

	  if (idx == nlines)
	    {
	      nlines += 2;
	      lines = (char **) xrealloc ((char *) lines,
					  nlines * sizeof (char *));
	    }
	  lines[idx++] = savestring (p, end - p);
	  p = end;
	  if (*p != '\0')
	    ++p;
	}

      if (idx != nlines)
	{
	  nlines = idx;
	  lines = (char **) xrealloc ((char *) lines,
				      nlines * sizeof (char *));
	}

      cmds->ncommand_lines = nlines;
      cmds->command_lines = lines;

      cmds->any_recurse = 0;
      cmds->lines_flags = (char *) xmalloc (nlines);
      for (idx = 0; idx < nlines; ++idx)
	{
	  int flags = 0;

	  for (p = lines[idx];
	       isblank (*p) || *p == '-' || *p == '@' || *p == '+';
	       ++p)
	    switch (*p)
	      {
	      case '+':
		flags |= COMMANDS_RECURSE;
		break;
	      case '@':
		flags |= COMMANDS_SILENT;
		break;
	      case '-':
		flags |= COMMANDS_NOERROR;
		break;
	      }
	  if (!(flags & COMMANDS_RECURSE))
	    {
	      unsigned int len = strlen (p);
	      if (sindex (p, len, "$(MAKE)", 7) != 0
		  || sindex (p, len, "${MAKE}", 7) != 0)
		flags |= COMMANDS_RECURSE;
	    }

	  cmds->lines_flags[idx] = flags;
	  cmds->any_recurse |= flags & COMMANDS_RECURSE;
	}
    }
}

/* Execute the commands to remake FILE.  If they are currently executing,
   return or have already finished executing, just return.  Otherwise,
   fork off a child process to run the first command line in the sequence.  */

void
execute_file_commands (file)
     struct file *file;
{
  register char *p;

  /* Don't go through all the preparations if
     the commands are nothing but whitespace.  */

  for (p = file->cmds->commands; *p != '\0'; ++p)
    if (!isspace (*p) && *p != '-' && *p != '@')
      break;
  if (*p == '\0')
    {
      /* We are all out of commands.
	 If we have gotten this far, all the previous commands
	 have run successfully, so we have winning update status.  */
      file->update_status = 0;
      notice_finished_file (file);
      return;
    }

  /* First set the automatic variables according to this file.  */

  initialize_file_variables (file);

  set_file_variables (file);

  /* Start the commands running.  */
  new_job (file);
}

/* This is set while we are inside fatal_error_signal,
   so things can avoid nonreentrant operations.  */

int handling_fatal_signal = 0;

/* Handle fatal signals.  */

RETSIGTYPE
fatal_error_signal (sig)
     int sig;
{
#if defined(__MSDOS__) || defined(_AMIGA)
  remove_intermediates (1);
#ifdef _AMIGA
  if (sig == SIGINT)
     fputs ("*** Break.\n", stderr);

  exit (10);
#else
  exit (1);
#endif
#else	/* Not MSDOS.  */
  handling_fatal_signal = 1;

  /* Set the handling for this signal to the default.
     It is blocked now while we run this handler.  */
  signal (sig, SIG_DFL);

  /* A termination signal won't be sent to the entire
     process group, but it means we want to kill the children.  */

  if (sig == SIGTERM)
    {
      register struct child *c;
      for (c = children; c != 0; c = c->next)
	if (!c->remote)
	  (void) kill (c->pid, SIGTERM);
    }

  /* If we got a signal that means the user
     wanted to kill make, remove pending targets.  */

  if (sig == SIGTERM || sig == SIGINT
#ifdef SIGHUP
    || sig == SIGHUP
#endif
#ifdef SIGQUIT
    || sig == SIGQUIT
#endif
    )
    {
      register struct child *c;

      /* Remote children won't automatically get signals sent
	 to the process group, so we must send them.  */
      for (c = children; c != 0; c = c->next)
	if (c->remote)
	  (void) remote_kill (c->pid, sig);

      for (c = children; c != 0; c = c->next)
	delete_child_targets (c);

      /* Clean up the children.  We don't just use the call below because
	 we don't want to print the "Waiting for children" message.  */
      while (job_slots_used > 0)
	reap_children (1, 0);
    }
  else
    /* Wait for our children to die.  */
    while (job_slots_used > 0)
      reap_children (1, 1);

  /* Delete any non-precious intermediate files that were made.  */

  remove_intermediates (1);

#ifdef SIGQUIT
  if (sig == SIGQUIT)
    /* We don't want to send ourselves SIGQUIT, because it will
       cause a core dump.  Just exit instead.  */
    exit (EXIT_FAILURE);
#endif

  /* Signal the same code; this time it will really be fatal.  The signal
     will be unblocked when we return and arrive then to kill us.  */
  if (kill (getpid (), sig) < 0)
    pfatal_with_name ("kill");
#endif	/* MSDOS.  */
}

/* Delete FILE unless it's precious or not actually a file (phony),
   and it has changed on disk since we last stat'd it.  */

static void
delete_target (file, on_behalf_of)
     struct file *file;
     char *on_behalf_of;
{
  struct stat st;

  if (file->precious || file->phony)
    return;

#ifndef NO_ARCHIVES
  if (ar_name (file->name))
    {
      if (ar_member_date (file->name) != file->last_mtime)
	{
	  if (on_behalf_of)
	    error ("*** [%s] Archive member `%s' may be bogus; not deleted",
		   on_behalf_of, file->name);
	  else
	    error ("*** Archive member `%s' may be bogus; not deleted",
		   file->name);
	}
      return;
    }
#endif

  if (stat (file->name, &st) == 0
      && S_ISREG (st.st_mode)
      && (time_t) st.st_mtime != file->last_mtime)
    {
      if (on_behalf_of)
	error ("*** [%s] Deleting file `%s'", on_behalf_of, file->name);
      else
	error ("*** Deleting file `%s'", file->name);
      if (unlink (file->name) < 0
	  && errno != ENOENT)	/* It disappeared; so what.  */
	perror_with_name ("unlink: ", file->name);
    }
}


/* Delete all non-precious targets of CHILD unless they were already deleted.
   Set the flag in CHILD to say they've been deleted.  */

void
delete_child_targets (child)
     struct child *child;
{
  struct dep *d;

  if (child->deleted)
    return;

  /* Delete the target file if it changed.  */
  delete_target (child->file, (char *) 0);

  /* Also remove any non-precious targets listed in the `also_make' member.  */
  for (d = child->file->also_make; d != 0; d = d->next)
    delete_target (d->file, child->file->name);

  child->deleted = 1;
}

/* Print out the commands in CMDS.  */

void
print_commands (cmds)
     register struct commands *cmds;
{
  register char *s;

  fputs ("#  commands to execute", stdout);

  if (cmds->filename == 0)
    puts (" (built-in):");
  else
    printf (" (from `%s', line %u):\n", cmds->filename, cmds->lineno);

  s = cmds->commands;
  while (*s != '\0')
    {
      char *end;

      while (isspace (*s))
	++s;

      end = index (s, '\n');
      if (end == 0)
	end = s + strlen (s);

      printf ("\t%.*s\n", (int) (end - s), s);

      s = end;
    }
}


/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         job.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Job execution and handling for GNU Make.
Copyright (C) 1988, 89, 90, 91, 92, 93, 94, 95, 96
	Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
#include "job.h"
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "commands.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "variable.h"  <- modification by J.Ruthruff, 7/20 */
#include <assert.h>

#undef stderr
#define stderr stdout

/* Default shell to use.  */
#ifdef WIN32
char *default_shell = "sh.exe";
int no_default_sh_exe = 1;
#else  /* WIN32 */
#ifndef _AMIGA
char default_shell[] = "/bin/sh";
#else
char default_shell[] = "";
extern int MyExecute (char **);
#endif
#endif /* WIN32 */

#ifdef __MSDOS__
#include <process.h>
#undef stderr
#define stderr stdout
static int dos_pid = 123;
static int dos_status;
static char *dos_bname;
static char *dos_bename;
static int dos_batch_file;
#endif /* MSDOS.  */

#ifdef _AMIGA
#include <proto/dos.h>
#undef stderr
#define stderr stdout
static int amiga_pid = 123;
static int amiga_status;
static char amiga_bname[32];
static int amiga_batch_file;
#endif /* Amiga.  */

#ifdef VMS
#include <time.h>
#include <processes.h>
#include <starlet.h>
#include <lib$routines.h>
#undef stderr
#define stderr stdout
#endif

#ifdef WIN32
#include <windows.h>
#include <io.h>
#include <process.h>
#include "sub_proc.h"
#include "w32err.h"
#include "pathstuff.h"
#undef stderr
#define stderr stdout

/* this stuff used if no sh.exe is around */
static char *dos_bname;
static char *dos_bename;
static int dos_batch_file;
#endif /* WIN32 */

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#undef stderr
#define stderr stdout
#else
#include <sys/file.h>
#undef stderr
#define stderr stdout
#endif

#if defined (HAVE_SYS_WAIT_H) || defined (HAVE_UNION_WAIT)
#include <sys/wait.h>
#undef stderr
#define stderr stdout
#endif

#ifdef	HAVE_WAITPID
#define	WAIT_NOHANG(status)	waitpid (-1, (status), WNOHANG)
#else	/* Don't have waitpid.  */
#ifdef	HAVE_WAIT3
#ifndef	wait3
extern int wait3 ();
#endif
#define	WAIT_NOHANG(status)	wait3 ((status), WNOHANG, (struct rusage *) 0)
#endif	/* Have wait3.  */
#endif	/* Have waitpid.  */

#if	!defined (wait) && !defined (POSIX)
extern int wait ();
#endif

#ifndef	HAVE_UNION_WAIT

#define	WAIT_T int

#ifndef	WTERMSIG
#define WTERMSIG(x) ((x) & 0x7f)
#endif
#ifndef	WCOREDUMP
#define WCOREDUMP(x) ((x) & 0x80)
#endif
#ifndef	WEXITSTATUS
#define WEXITSTATUS(x) (((x) >> 8) & 0xff)
#endif
#ifndef	WIFSIGNALED
#define WIFSIGNALED(x) (WTERMSIG (x) != 0)
#endif
#ifndef	WIFEXITED
#define WIFEXITED(x) (WTERMSIG (x) == 0)
#endif

#else	/* Have `union wait'.  */

#define WAIT_T union wait
#ifndef	WTERMSIG
#define WTERMSIG(x)	((x).w_termsig)
#endif
#ifndef	WCOREDUMP
#define WCOREDUMP(x)	((x).w_coredump)
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(x)	((x).w_retcode)
#endif
#ifndef	WIFSIGNALED
#define	WIFSIGNALED(x)	(WTERMSIG(x) != 0)
#endif
#ifndef	WIFEXITED
#define	WIFEXITED(x)	(WTERMSIG(x) == 0)
#endif

#endif	/* Don't have `union wait'.  */

#ifdef VMS
static int vms_jobsefnmask=0;
#endif /* !VMS */

#ifndef	HAVE_UNISTD_H
extern int dup2 ();
extern int execve ();
extern void _exit ();
#ifndef VMS
extern int geteuid ();
extern int getegid ();
extern int setgid ();
extern int getgid ();
#endif
#endif

extern char *allocated_variable_expand_for_file PARAMS ((char *line, struct file *file));

extern int getloadavg PARAMS ((double loadavg[], int nelem));
extern int start_remote_job PARAMS ((char **argv, char **envp, int stdin_fd,
		int *is_remote, int *id_ptr, int *used_stdin));
extern int start_remote_job_p PARAMS ((void));
extern int remote_status PARAMS ((int *exit_code_ptr, int *signal_ptr,
		int *coredump_ptr, int block));

RETSIGTYPE child_handler PARAMS ((int));
static void free_child PARAMS ((struct child *));
static void start_job_command PARAMS ((struct child *child));
static int load_too_high PARAMS ((void));
static int job_next_command PARAMS ((struct child *));
static int start_waiting_job PARAMS ((struct child *));
#ifdef VMS
static void vmsWaitForChildren PARAMS ((int *));
#endif

/* Chain of all live (or recently deceased) children.  */

struct child *children = 0;

/* Number of children currently running.  */

unsigned int job_slots_used = 0;

/* Nonzero if the `good' standard input is in use.  */

static int good_stdin_used = 0;

/* Chain of children waiting to run until the load average goes down.  */

static struct child *waiting_jobs = 0;

#ifdef WIN32
/*
 * The macro which references this function is defined in make.h.
 */
int w32_kill(int pid, int sig)
{
       return ((process_kill(pid, sig) == TRUE) ? 0 : -1);
}
#endif /* WIN32 */

/* Write an error message describing the exit status given in
   EXIT_CODE, EXIT_SIG, and COREDUMP, for the target TARGET_NAME.
   Append "(ignored)" if IGNORED is nonzero.  */

static void
child_error (target_name, exit_code, exit_sig, coredump, ignored)
     char *target_name;
     int exit_code, exit_sig, coredump;
     int ignored;
{
  if (ignored && silent_flag)
    return;

#ifdef VMS
  if (!(exit_code & 1))
      error("*** [%s] Error 0x%x%s", target_name, exit_code, ((ignored)? " (ignored)" : ""));
#else
  if (exit_sig == 0)
    error (ignored ? "[%s] Error %d (ignored)" :
	   "*** [%s] Error %d",
	   target_name, exit_code);
  else
    error ("*** [%s] %s%s",
	   target_name, strsignal (exit_sig),
	   coredump ? " (core dumped)" : "");
#endif /* VMS */
}

static unsigned int dead_children = 0;

#ifdef VMS
/* Wait for nchildren children to terminate */
static void
vmsWaitForChildren(int *status)
{
  while (1)
    {
      if (!vms_jobsefnmask)
	{
	  *status = 0;
	  return;
	}

      *status = sys$wflor (32, vms_jobsefnmask);
    }
  return;
}
#endif


/* Notice that a child died.
   reap_children should be called when convenient.  */
RETSIGTYPE
child_handler (sig)
     int sig;
{
  ++dead_children;

  if (debug_flag)
    printf ("Got a SIGCHLD; %d unreaped children.\n", dead_children);
}

extern int shell_function_pid, shell_function_completed;

/* Reap dead children, storing the returned status and the new command
   state (`cs_finished') in the `file' member of the `struct child' for the
   dead child, and removing the child from the chain.  If BLOCK nonzero,
   reap at least one child, waiting for it to die if necessary.  If ERR is
   nonzero, print an error message first.  */

void
reap_children (block, err)
     int block, err;
{
  WAIT_T status;

  while ((children != 0 || shell_function_pid != 0) &&
	 (block || dead_children > 0))
    {
      int remote = 0;
      register int pid;
      int exit_code, exit_sig, coredump;
      register struct child *lastc, *c;
      int child_failed;
      int any_remote, any_local;

      if (err && dead_children == 0)
	{
	  /* We might block for a while, so let the user know why.  */
	  fflush (stdout);
	  error ("*** Waiting for unfinished jobs....");
	}

      /* We have one less dead child to reap.
	 The test and decrement are not atomic; if it is compiled into:
	 	register = dead_children - 1;
		dead_children = register;
	 a SIGCHLD could come between the two instructions.
	 child_handler increments dead_children.
	 The second instruction here would lose that increment.  But the
	 only effect of dead_children being wrong is that we might wait
	 longer than necessary to reap a child, and lose some parallelism;
	 and we might print the "Waiting for unfinished jobs" message above
	 when not necessary.  */

      if (dead_children > 0)
	--dead_children;

      any_remote = 0;
      any_local = shell_function_pid != 0;
      for (c = children; c != 0; c = c->next)
	{
	  any_remote |= c->remote;
	  any_local |= ! c->remote;
	  if (debug_flag)
	    printf ("Live child 0x%08lx PID %d%s\n",
		    (unsigned long int) c,
		    c->pid, c->remote ? " (remote)" : "");
#ifdef VMS
	  break;
#endif
	}

      /* First, check for remote children.  */
      if (any_remote)
	pid = remote_status (&exit_code, &exit_sig, &coredump, 0);
      else
	pid = 0;

      if (pid < 0)
	{
	remote_status_lose:
#ifdef	EINTR
	  if (errno == EINTR)
	    continue;
#endif
	  pfatal_with_name ("remote_status");
	}
      else if (pid == 0)
	{
#if !defined(__MSDOS__) && !defined(_AMIGA) && !defined(WIN32)
	  /* No remote children.  Check for local children.  */

	  if (any_local)
	    {
#ifdef VMS
	      vmsWaitForChildren (&status);
	      pid = c->pid;
#else
#ifdef WAIT_NOHANG
	      if (!block)
		pid = WAIT_NOHANG (&status);
	      else
#endif
		pid = wait (&status);
#endif /* !VMS */
	    }
	  else
	    pid = 0;

	  if (pid < 0)
	    {
#ifdef EINTR
	      if (errno == EINTR)
		continue;
#endif
	      pfatal_with_name ("wait");
	    }
	  else if (pid == 0)
	    {
	      /* No local children.  */
	      if (block && any_remote)
		{
		  /* Now try a blocking wait for a remote child.  */
		  pid = remote_status (&exit_code, &exit_sig, &coredump, 1);
		  if (pid < 0)
		    goto remote_status_lose;
		  else if (pid == 0)
		    /* No remote children either.  Finally give up.  */
		    break;
		  else
		    /* We got a remote child.  */
		    remote = 1;
		}
	      else
		break;
	    }
	  else
	    {
	      /* Chop the status word up.  */
	      exit_code = WEXITSTATUS (status);
	      exit_sig = WIFSIGNALED (status) ? WTERMSIG (status) : 0;
	      coredump = WCOREDUMP (status);
	    }
#else	/* MSDOS, Amiga, WIN32.  */
#ifdef __MSDOS__
	  /* Life is very different on MSDOS.  */
	  pid = dos_pid - 1;
	  status = dos_status;
	  exit_code = dos_status;
	  exit_sig = 0;
	  coredump = 0;
#endif /* __MSDOS__ */
#ifdef _AMIGA
	  /* Same on Amiga */
	  pid = amiga_pid - 1;
	  status = amiga_status;
	  exit_code = amiga_status;
	  exit_sig = 0;
	  coredump = 0;
#endif /* _AMIGA */
#ifdef WIN32
         {
           HANDLE hPID;
           int err;

           /* wait for anything to finish */
           if (hPID = process_wait_for_any()) {

             /* was an error found on this process? */
             err = process_last_err(hPID);

             /* get exit data */
             exit_code = process_exit_code(hPID);

             if (err)
               fprintf(stderr, "make (e=%d): %s",
                       exit_code, map_win32_error_to_string(exit_code));

             exit_sig = process_signal(hPID);

             /* cleanup process */
             process_cleanup(hPID);

             if (dos_batch_file) {
               remove (dos_bname);
               remove (dos_bename);
               dos_batch_file = 0;
             }

             coredump = 0;
           }
           pid = (int) hPID;
         }
#endif /* WIN32 */
#endif	/* Not MSDOS.  */
	}
      else
	/* We got a remote child.  */
	remote = 1;

      /* Check if this is the child of the `shell' function.  */
      if (!remote && pid == shell_function_pid)
	{
	  /* It is.  Leave an indicator for the `shell' function.  */
	  if (exit_sig == 0 && exit_code == 127)
	    shell_function_completed = -1;
	  else
	    shell_function_completed = 1;
	  break;
	}

      child_failed = exit_sig != 0 || exit_code != 0;

      /* Search for a child matching the deceased one.  */
      lastc = 0;
      for (c = children; c != 0; lastc = c, c = c->next)
	if (c->remote == remote && c->pid == pid)
	  break;

      if (c == 0)
	{
	  /* An unknown child died.  */
	  char buf[100];
	  sprintf (buf, "Unknown%s job %d", remote ? " remote" : "", pid);
	  if (child_failed)
	    child_error (buf, exit_code, exit_sig, coredump,
			 ignore_errors_flag);
	  else
	    error ("%s finished.", buf);
	}
      else
	{
	  if (debug_flag)
	    printf ("Reaping %s child 0x%08lx PID %d%s\n",
		    child_failed ? "losing" : "winning",
		    (unsigned long int) c,
		    c->pid, c->remote ? " (remote)" : "");

	  /* If this child had the good stdin, say it is now free.  */
	  if (c->good_stdin)
	    good_stdin_used = 0;

	  if (child_failed && !c->noerror && !ignore_errors_flag)
	    {
	      /* The commands failed.  Write an error message,
		 delete non-precious targets, and abort.  */
	      static int delete_on_error = -1;
	      child_error (c->file->name, exit_code, exit_sig, coredump, 0);
	      c->file->update_status = 2;
	      if (delete_on_error == -1)
		{
		  struct file *f = lookup_file (".DELETE_ON_ERROR");
		  delete_on_error = f != 0 && f->is_target;
		}
	      if (exit_sig != 0 || delete_on_error)
		delete_child_targets (c);
	    }
	  else
	    {
	      if (child_failed)
		{
		  /* The commands failed, but we don't care.  */
		  child_error (c->file->name,
			       exit_code, exit_sig, coredump, 1);
		  child_failed = 0;
		}

	      /* If there are more commands to run, try to start them.  */
	      if (job_next_command (c))
		{
		  if (handling_fatal_signal)
		    {
		      /* Never start new commands while we are dying.
			 Since there are more commands that wanted to be run,
			 the target was not completely remade.  So we treat
			 this as if a command had failed.  */
		      c->file->update_status = 2;
		    }
		  else
		    {
		      /* Check again whether to start remotely.
			 Whether or not we want to changes over time.
			 Also, start_remote_job may need state set up
			 by start_remote_job_p.  */
		      c->remote = start_remote_job_p ();
		      start_job_command (c);
		      /* Fatal signals are left blocked in case we were
			 about to put that child on the chain.  But it is
			 already there, so it is safe for a fatal signal to
			 arrive now; it will clean up this child's targets.  */
		      unblock_sigs ();
		      if (c->file->command_state == cs_running)
			/* We successfully started the new command.
			   Loop to reap more children.  */
			continue;
		    }

		  if (c->file->update_status != 0)
		    /* We failed to start the commands.  */
		    delete_child_targets (c);
		}
	      else
		/* There are no more commands.  We got through them all
		   without an unignored error.  Now the target has been
		   successfully updated.  */
		c->file->update_status = 0;
	    }

	  /* When we get here, all the commands for C->file are finished
	     (or aborted) and C->file->update_status contains 0 or 2.  But
	     C->file->command_state is still cs_running if all the commands
	     ran; notice_finish_file looks for cs_running to tell it that
	     it's interesting to check the file's modtime again now.  */

	  if (! handling_fatal_signal)
	    /* Notice if the target of the commands has been changed.
	       This also propagates its values for command_state and
	       update_status to its also_make files.  */
	    notice_finished_file (c->file);

	  if (debug_flag)
	    printf ("Removing child 0x%08lx PID %d%s from chain.\n",
		    (unsigned long int) c,
		    c->pid, c->remote ? " (remote)" : "");

	  /* Block fatal signals while frobnicating the list, so that
	     children and job_slots_used are always consistent.  Otherwise
	     a fatal signal arriving after the child is off the chain and
	     before job_slots_used is decremented would believe a child was
	     live and call reap_children again.  */
	  block_sigs ();

	  /* Remove the child from the chain and free it.  */
	  if (lastc == 0)
	    children = c->next;
	  else
	    lastc->next = c->next;
	  if (! handling_fatal_signal) /* Don't bother if about to die.  */
	    free_child (c);

	  /* There is now another slot open.  */
	  if (job_slots_used > 0)
	    --job_slots_used;

	  unblock_sigs ();

	  /* If the job failed, and the -k flag was not given, die,
	     unless we are already in the process of dying.  */
	  if (!err && child_failed && !keep_going_flag &&
	      /* fatal_error_signal will die with the right signal.  */
	      !handling_fatal_signal)
	    die (2);
	}

      /* Only block for one child.  */
      block = 0;
    }
  return;
}

/* Free the storage allocated for CHILD.  */

static void
free_child (child)
     register struct child *child;
{
  if (child->command_lines != 0)
    {
      register unsigned int i;
      for (i = 0; i < child->file->cmds->ncommand_lines; ++i)
	free (child->command_lines[i]);
      free ((char *) child->command_lines);
    }

  if (child->environment != 0)
    {
      register char **ep = child->environment;
      while (*ep != 0)
	free (*ep++);
      free ((char *) child->environment);
    }

  free ((char *) child);
}

#ifdef POSIX
extern sigset_t fatal_signal_set;
#endif

void
block_sigs ()
{
#ifdef	 POSIX
  (void) sigprocmask (SIG_BLOCK, &fatal_signal_set, (sigset_t *) 0);
#else
#ifdef	HAVE_SIGSETMASK
  (void) sigblock (fatal_signal_mask);
#endif
#endif
}

#ifdef	POSIX
#ifdef	__MSDOS__
void
unblock_sigs ()
{
  return;
}
#else
void
unblock_sigs ()
{
  sigset_t empty;
  sigemptyset (&empty);
  sigprocmask (SIG_SETMASK, &empty, (sigset_t *) 0);
}
#endif
#endif

/* Start a job to run the commands specified in CHILD.
   CHILD is updated to reflect the commands and ID of the child process.

   NOTE: On return fatal signals are blocked!  The caller is responsible
   for calling `unblock_sigs', once the new child is safely on the chain so
   it can be cleaned up in the event of a fatal signal.  */

static void
start_job_command (child)
     register struct child *child;
{
#ifndef _AMIGA
  static int bad_stdin = -1;
#endif
  register char *p;
  int flags;
#ifdef VMS
  char *argv;
#else
  char **argv;
#endif

  /* Combine the flags parsed for the line itself with
     the flags specified globally for this target.  */
  flags = (child->file->command_flags
	   | child->file->cmds->lines_flags[child->command_line - 1]);

  p = child->command_ptr;
  child->noerror = flags & COMMANDS_NOERROR;

  while (*p != '\0')
    {
      if (*p == '@')
	flags |= COMMANDS_SILENT;
      else if (*p == '+')
	flags |= COMMANDS_RECURSE;
      else if (*p == '-')
	child->noerror = 1;
      else if (!isblank (*p) && *p != '+')
	break;
      ++p;
    }

  /* If -q was given, just say that updating `failed'.  The exit status of
     1 tells the user that -q is saying `something to do'; the exit status
     for a random error is 2.  */
  if (question_flag && !(flags & COMMANDS_RECURSE))
    {
      child->file->update_status = 1;
      notice_finished_file (child->file);
      return;
    }

  /* There may be some preceding whitespace left if there
     was nothing but a backslash on the first line.  */
  p = next_token (p);

  /* Figure out an argument list from this command line.  */

  {
    char *end = 0;
#ifdef VMS
    argv = p;
#else
    argv = construct_command_argv (p, &end, child->file);
#endif
    if (end == NULL)
      child->command_ptr = NULL;
    else
      {
	*end++ = '\0';
	child->command_ptr = end;
      }
  }

  if (touch_flag && !(flags & COMMANDS_RECURSE))
    {
      /* Go on to the next command.  It might be the recursive one.
	 We construct ARGV only to find the end of the command line.  */
#ifndef VMS
      free (argv[0]);
      free ((char *) argv);
#endif
      argv = 0;
    }

  if (argv == 0)
    {
    next_command:
      /* This line has no commands.  Go to the next.  */
      if (job_next_command (child))
	start_job_command (child);
      else
	{
	  /* No more commands.  All done.  */
	  child->file->update_status = 0;
	  notice_finished_file (child->file);
	}
      return;
    }

  /* Print out the command.  If silent, we call `message' with null so it
     can log the working directory before the command's own error messages
     appear.  */

  message (0, (just_print_flag || (!(flags & COMMANDS_SILENT) && !silent_flag))
	   ? "%s" : (char *) 0, p);

  /* Tell update_goal_chain that a command has been started on behalf of
     this target.  It is important that this happens here and not in
     reap_children (where we used to do it), because reap_children might be
     reaping children from a different target.  We want this increment to
     guaranteedly indicate that a command was started for the dependency
     chain (i.e., update_file recursion chain) we are processing.  */

  ++commands_started;

  /* If -n was given, recurse to get the next line in the sequence.  */

  if (just_print_flag && !(flags & COMMANDS_RECURSE))
    {
#ifndef VMS
      free (argv[0]);
      free ((char *) argv);
#endif
      goto next_command;
    }

  /* Flush the output streams so they won't have things written twice.  */

  fflush (stdout);
  fflush (stderr);

#ifndef WIN32
#ifndef _AMIGA
#ifndef VMS

  /* Set up a bad standard input that reads from a broken pipe.  */

  if (bad_stdin == -1)
    {
      /* Make a file descriptor that is the read end of a broken pipe.
	 This will be used for some children's standard inputs.  */
      int pd[2];
      if (pipe (pd) == 0)
	{
	  /* Close the write side.  */
	  (void) close (pd[1]);
	  /* Save the read side.  */
	  bad_stdin = pd[0];

	  /* Set the descriptor to close on exec, so it does not litter any
	     child's descriptor table.  When it is dup2'd onto descriptor 0,
	     that descriptor will not close on exec.  */
#ifdef FD_SETFD
#ifndef FD_CLOEXEC
#define FD_CLOEXEC 1
#endif
	  (void) fcntl (bad_stdin, F_SETFD, FD_CLOEXEC);
#endif
	}
    }

#endif /* !AMIGA */
#endif /* !WIN32 */

  /* Decide whether to give this child the `good' standard input
     (one that points to the terminal or whatever), or the `bad' one
     that points to the read side of a broken pipe.  */

  child->good_stdin = !good_stdin_used;
  if (child->good_stdin)
    good_stdin_used = 1;

#endif /* Not VMS */

  child->deleted = 0;

#ifndef _AMIGA
  /* Set up the environment for the child.  */
  if (child->environment == 0)
    child->environment = target_environment (child->file);
#endif

#if !defined(__MSDOS__) && !defined(_AMIGA) && !defined(WIN32)

#ifndef VMS
  /* start_waiting_job has set CHILD->remote if we can start a remote job.  */
  if (child->remote)
    {
      int is_remote, id, used_stdin;
      if (start_remote_job (argv, child->environment,
			    child->good_stdin ? 0 : bad_stdin,
			    &is_remote, &id, &used_stdin))
	goto error;
      else
	{
	  if (child->good_stdin && !used_stdin)
	    {
	      child->good_stdin = 0;
	      good_stdin_used = 0;
	    }
	  child->remote = is_remote;
	  child->pid = id;
	}
    }
  else
#endif /* !VMS */
    {
      /* Fork the child process.  */

      char **parent_environ;

      block_sigs ();

      child->remote = 0;

#ifdef VMS

      if (!child_execute_job (argv, child)) {
        /* Fork failed!  */
        perror_with_name ("vfork", "");
        goto error;
      }

#else

      parent_environ = environ;
      child->pid = vfork ();
      environ = parent_environ;	/* Restore value child may have clobbered.  */
      if (child->pid == 0)
	{
	  /* We are the child side.  */
	  unblock_sigs ();
	  child_execute_job (child->good_stdin ? 0 : bad_stdin, 1,
			     argv, child->environment);
	}
      else if (child->pid < 0)
	{
	  /* Fork failed!  */
	  unblock_sigs ();
	  perror_with_name ("vfork", "");
	  goto error;
	}
#endif /* !VMS */
    }

#else	/* MSDOS or Amiga.  */
#ifdef __MSDOS__
  dos_status = spawnvpe (P_WAIT, argv[0], argv, child->environment);
  ++dead_children;
  child->pid = dos_pid++;
  if (dos_batch_file)
   {
     dos_batch_file = 0;
     remove (dos_bname);	/* Ignore errors.  */
     if (access (dos_bename, 0))
       dos_status = 1;
     else
       dos_status = 0;
     remove (dos_bename);
   }
#endif /* __MSDOS__ */
#ifdef _AMIGA
  amiga_status = MyExecute (argv);

  ++dead_children;
  child->pid = amiga_pid++;
  if (amiga_batch_file)
  {
     amiga_batch_file = 0;
     DeleteFile (amiga_bname);        /* Ignore errors.  */
  }
#endif	/* Not Amiga */
#ifdef WIN32
  {
      HANDLE hPID;
      char* arg0;

      /* make UNC paths safe for CreateProcess -- backslash format */
      arg0 = argv[0];
      if (arg0 && arg0[0] == '/' && arg0[1] == '/')
        for ( ; arg0 && *arg0; arg0++)
          if (*arg0 == '/')
            *arg0 = '\\';

      /* make sure CreateProcess() has Path it needs */
      sync_Path_environment();

      hPID = process_easy(argv, child->environment);

      if (hPID != INVALID_HANDLE_VALUE)
        child->pid = (int) hPID;
      else {
        int i;
        unblock_sigs();
        fprintf(stderr,
          "process_easy() failed failed to launch process (e=%d)\n",
          process_last_err(hPID));
               for (i = 0; argv[i]; i++)
                 fprintf(stderr, "%s ", argv[i]);
               fprintf(stderr, "\nCounted %d args in failed launch\n", i);
      }
  }
#endif /* WIN32 */
#endif	/* Not MSDOS.  */

  /* We are the parent side.  Set the state to
     say the commands are running and return.  */

  set_command_state (child->file, cs_running);

  /* Free the storage used by the child's argument list.  */
#ifndef VMS
  free (argv[0]);
  free ((char *) argv);
#endif

  return;

 error:
  child->file->update_status = 2;
  notice_finished_file (child->file);
  return;
}

/* Try to start a child running.
   Returns nonzero if the child was started (and maybe finished), or zero if
   the load was too high and the child was put on the `waiting_jobs' chain.  */

static int
start_waiting_job (c)
     struct child *c;
{
  /* If we can start a job remotely, we always want to, and don't care about
     the local load average.  We record that the job should be started
     remotely in C->remote for start_job_command to test.  */

  c->remote = start_remote_job_p ();

  /* If this job is to be started locally, and we are already running
     some jobs, make this one wait if the load average is too high.  */
  if (!c->remote && job_slots_used > 0 && load_too_high ())
    {
      /* Put this child on the chain of children waiting
	 for the load average to go down.  */
      set_command_state (c->file, cs_running);
      c->next = waiting_jobs;
      waiting_jobs = c;
      return 0;
    }

  /* Start the first command; reap_children will run later command lines.  */
  start_job_command (c);

  switch (c->file->command_state)
    {
    case cs_running:
      c->next = children;
      if (debug_flag)
	printf ("Putting child 0x%08lx PID %05d%s on the chain.\n",
		(unsigned long int) c,
		c->pid, c->remote ? " (remote)" : "");
      children = c;
      /* One more job slot is in use.  */
      ++job_slots_used;
      unblock_sigs ();
      break;

    case cs_not_started:
      /* All the command lines turned out to be empty.  */
      c->file->update_status = 0;
      /* FALLTHROUGH */

    case cs_finished:
      notice_finished_file (c->file);
      free_child (c);
      break;

    default:
      assert (c->file->command_state == cs_finished);
      break;
    }

  return 1;
}

/* Create a `struct child' for FILE and start its commands running.  */

void
new_job (file)
     register struct file *file;
{
  register struct commands *cmds = file->cmds;
  register struct child *c;
  char **lines;
  register unsigned int i;

  /* Let any previously decided-upon jobs that are waiting
     for the load to go down start before this new one.  */
  start_waiting_jobs ();

  /* Reap any children that might have finished recently.  */
  reap_children (0, 0);

  /* Chop the commands up into lines if they aren't already.  */
  chop_commands (cmds);

  if (job_slots != 0)
    /* Wait for a job slot to be freed up.  */
    while (job_slots_used == job_slots)
      reap_children (1, 0);

  /* Expand the command lines and store the results in LINES.  */
  lines = (char **) xmalloc (cmds->ncommand_lines * sizeof (char *));
  for (i = 0; i < cmds->ncommand_lines; ++i)
    {
      /* Collapse backslash-newline combinations that are inside variable
	 or function references.  These are left alone by the parser so
	 that they will appear in the echoing of commands (where they look
	 nice); and collapsed by construct_command_argv when it tokenizes.
	 But letting them survive inside function invocations loses because
	 we don't want the functions to see them as part of the text.  */

      char *in, *out, *ref;

      /* IN points to where in the line we are scanning.
	 OUT points to where in the line we are writing.
	 When we collapse a backslash-newline combination,
	 IN gets ahead out OUT.  */

      in = out = cmds->command_lines[i];
      while ((ref = index (in, '$')) != 0)
	{
	  ++ref;		/* Move past the $.  */

	  if (out != in)
	    /* Copy the text between the end of the last chunk
	       we processed (where IN points) and the new chunk
	       we are about to process (where REF points).  */
	    bcopy (in, out, ref - in);

	  /* Move both pointers past the boring stuff.  */
	  out += ref - in;
	  in = ref;

	  if (*ref == '(' || *ref == '{')
	    {
	      char openparen = *ref;
	      char closeparen = openparen == '(' ? ')' : '}';
	      int count;
	      char *p;

	      *out++ = *in++;	/* Copy OPENPAREN.  */
	      /* IN now points past the opening paren or brace.
		 Count parens or braces until it is matched.  */
	      count = 0;
	      while (*in != '\0')
		{
		  if (*in == closeparen && --count < 0)
		    break;
		  else if (*in == '\\' && in[1] == '\n')
		    {
		      /* We have found a backslash-newline inside a
			 variable or function reference.  Eat it and
			 any following whitespace.  */

		      int quoted = 0;
		      for (p = in - 1; p > ref && *p == '\\'; --p)
			quoted = !quoted;

		      if (quoted)
			/* There were two or more backslashes, so this is
			   not really a continuation line.  We don't collapse
			   the quoting backslashes here as is done in
			   collapse_continuations, because the line will
			   be collapsed again after expansion.  */
			*out++ = *in++;
		      else
			{
			  /* Skip the backslash, newline and
			     any following whitespace.  */
			  in = next_token (in + 2);

			  /* Discard any preceding whitespace that has
			     already been written to the output.  */
			  while (out > ref && isblank (out[-1]))
			    --out;

			  /* Replace it all with a single space.  */
			  *out++ = ' ';
			}
		    }
		  else
		    {
		      if (*in == openparen)
			++count;

		      *out++ = *in++;
		    }
		}
	    }
	}

      /* There are no more references in this line to worry about.
	 Copy the remaining uninteresting text to the output.  */
      if (out != in)
	strcpy (out, in);

      /* Finally, expand the line.  */
      lines[i] = allocated_variable_expand_for_file (cmds->command_lines[i],
						     file);
    }

  /* Start the command sequence, record it in a new
     `struct child', and add that to the chain.  */

  c = (struct child *) xmalloc (sizeof (struct child));
  c->file = file;
  c->command_lines = lines;
  c->command_line = 0;
  c->command_ptr = 0;
  c->environment = 0;

  /* Fetch the first command line to be run.  */
  job_next_command (c);

  /* The job is now primed.  Start it running.
     (This will notice if there are in fact no commands.)  */
  (void)start_waiting_job (c);

  if (job_slots == 1)
    /* Since there is only one job slot, make things run linearly.
       Wait for the child to die, setting the state to `cs_finished'.  */
    while (file->command_state == cs_running)
      reap_children (1, 0);

  return;
}

/* Move CHILD's pointers to the next command for it to execute.
   Returns nonzero if there is another command.  */

static int
job_next_command (child)
     struct child *child;
{
  while (child->command_ptr == 0 || *child->command_ptr == '\0')
    {
      /* There are no more lines in the expansion of this line.  */
      if (child->command_line == child->file->cmds->ncommand_lines)
	{
	  /* There are no more lines to be expanded.  */
	  child->command_ptr = 0;
	  return 0;
	}
      else
	/* Get the next line to run.  */
	child->command_ptr = child->command_lines[child->command_line++];
    }
  return 1;
}

static int
load_too_high ()
{
#if defined(__MSDOS__) || defined(VMS) || defined(_AMIGA)
  return 1;
#else
  double load;

  if (max_load_average < 0)
    return 0;

  make_access ();
  if (getloadavg (&load, 1) != 1)
    {
      static int lossage = -1;
      /* Complain only once for the same error.  */
      if (lossage == -1 || errno != lossage)
	{
	  if (errno == 0)
	    /* An errno value of zero means getloadavg is just unsupported.  */
	    error ("cannot enforce load limits on this operating system");
	  else
	    perror_with_name ("cannot enforce load limit: ", "getloadavg");
	}
      lossage = errno;
      load = 0;
    }
  user_access ();

  return load >= max_load_average;
#endif
}

/* Start jobs that are waiting for the load to be lower.  */

void
start_waiting_jobs ()
{
  struct child *job;

  if (waiting_jobs == 0)
    return;

  do
    {
      /* Check for recently deceased descendants.  */
      reap_children (0, 0);

      /* Take a job off the waiting list.  */
      job = waiting_jobs;
      waiting_jobs = job->next;

      /* Try to start that job.  We break out of the loop as soon
	 as start_waiting_job puts one back on the waiting list.  */
    }
  while (start_waiting_job (job) && waiting_jobs != 0);

  return;
}

#ifndef WIN32
#ifdef VMS
#include <descrip.h>
#include <clidef.h>
#undef stderr
#define stderr stdout

/* This is called as an AST when a child process dies (it won't get
   interrupted by anything except a higher level AST).
*/
int vmsHandleChildTerm(struct child *child)
{
    int status;
    register struct child *lastc, *c;
    int child_failed;

    vms_jobsefnmask &= ~(1 << (child->efn - 32));

    lib$free_ef(&child->efn);

    (void) sigblock (fatal_signal_mask);

    child_failed = !(child->cstatus & 1 || ((child->cstatus & 7) == 0));

    /* Search for a child matching the deceased one.  */
    lastc = 0;
#if defined(RECURSIVEJOBS) /* I've had problems with recursive stuff and process handling */
    for (c = children; c != 0 && c != child; lastc = c, c = c->next);
#else
    c = child;
#endif

    if (child_failed && !c->noerror && !ignore_errors_flag)
      {
	/* The commands failed.  Write an error message,
	   delete non-precious targets, and abort.  */
	child_error (c->file->name, c->cstatus, 0, 0, 0);
	c->file->update_status = 1;
	delete_child_targets (c);
      }
    else
      {
	if (child_failed)
	  {
	    /* The commands failed, but we don't care.  */
	    child_error (c->file->name, c->cstatus, 0, 0, 1);
	    child_failed = 0;
	  }

#if defined(RECURSIVEJOBS) /* I've had problems with recursive stuff and process handling */
	/* If there are more commands to run, try to start them.  */
	start_job (c);

	switch (c->file->command_state)
	  {
	  case cs_running:
	    /* Successfully started.  */
	    break;

	  case cs_finished:
	    if (c->file->update_status != 0) {
		/* We failed to start the commands.  */
		delete_child_targets (c);
	    }
	    break;

	  default:
	    error ("internal error: `%s' command_state \
%d in child_handler", c->file->name);
	    abort ();
	    break;
	  }
#endif /* RECURSIVEJOBS */
      }

    /* Set the state flag to say the commands have finished.  */
    c->file->command_state = cs_finished;
    notice_finished_file (c->file);

#if defined(RECURSIVEJOBS) /* I've had problems with recursive stuff and process handling */
    /* Remove the child from the chain and free it.  */
    if (lastc == 0)
      children = c->next;
    else
      lastc->next = c->next;
    free_child (c);
#endif /* RECURSIVEJOBS */

    /* There is now another slot open.  */
    if (job_slots_used > 0)
      --job_slots_used;

    /* If the job failed, and the -k flag was not given, die.  */
    if (child_failed && !keep_going_flag)
      die (EXIT_FAILURE);

    (void) sigsetmask (sigblock (0) & ~(fatal_signal_mask));

    return 1;
}

/* VMS:
   Spawn a process executing the command in ARGV and return its pid. */

#define MAXCMDLEN 200

int
child_execute_job (argv, child)
     char *argv;
     struct child *child;
{
  int i;
  static struct dsc$descriptor_s cmddsc;
#ifndef DONTWAITFORCHILD
  int spflags = 0;
#else
  int spflags = CLI$M_NOWAIT;
#endif
  int status;
  char cmd[4096],*p,*c;
  char comname[50];

/* Remove backslashes */
  for (p = argv, c = cmd; *p; p++,c++)
    {
      if (*p == '\\') p++;
	*c = *p;
    }
  *c = *p;

  /* check for maximum dcl length and create *.com file if neccesary */

  comname[0] = '\0';

  if (strlen (cmd) > MAXCMDLEN)
    {
      FILE *outfile;
      char tmp;

      strcpy (comname, "sys$scratch:CMDXXXXXX.COM");
      (void) mktemp (comname);

      outfile = fopen (comname, "w");
      if (outfile == 0)
	pfatal_with_name (comname);

      fprintf (outfile, "$ ");
      c = cmd;

      while (c)
	{
	  p = strchr (c, ',');
	  if ((p == NULL) || (p-c > MAXCMDLEN))
	    p = strchr (c, ' ');
	  if (p != NULL)
	    {
	      p++;
	      tmp = *p;
	      *p = '\0';
	    }
	  else
	    tmp = '\0';
	  fprintf (outfile, "%s%s\n", c, (tmp == '\0')?"":" -");
	  if (p != NULL)
	    *p = tmp;
	  c = p;
	}

      fclose (outfile);

      sprintf (cmd, "$ @%s", comname);

      if (debug_flag)
	printf ("Executing %s instead\n", cmd);
    }

  cmddsc.dsc$w_length = strlen(cmd);
  cmddsc.dsc$a_pointer = cmd;
  cmddsc.dsc$b_dtype = DSC$K_DTYPE_T;
  cmddsc.dsc$b_class = DSC$K_CLASS_S;

  child->efn = 0;
  while (child->efn < 32 || child->efn > 63)
    {
      status = lib$get_ef(&child->efn);
      if (!(status & 1))
	return 0;
    }

  sys$clref(child->efn);

  vms_jobsefnmask |= (1 << (child->efn - 32));

#ifndef DONTWAITFORCHILD
  status = lib$spawn(&cmddsc,0,0,&spflags,0,&child->pid,&child->cstatus,
		       &child->efn,0,0);
  vmsHandleChildTerm(child);
#else
  status = lib$spawn(&cmddsc,0,0,&spflags,0,&child->pid,&child->cstatus,
		       &child->efn,vmsHandleChildTerm,child);
#endif

  if (!(status & 1))
    {
      printf("Error spawning, %d\n",status);
      fflush(stdout);
    }

  unlink (comname);

  return (status & 1);
}

#else /* !VMS */

#ifndef _AMIGA
/* UNIX:
   Replace the current process with one executing the command in ARGV.
   STDIN_FD and STDOUT_FD are used as the process's stdin and stdout; ENVP is
   the environment of the new program.  This function does not return.  */

void
child_execute_job (stdin_fd, stdout_fd, argv, envp)
     int stdin_fd, stdout_fd;
     char **argv, **envp;
{
  if (stdin_fd != 0)
    (void) dup2 (stdin_fd, 0);
  if (stdout_fd != 1)
    (void) dup2 (stdout_fd, 1);
  if (stdin_fd != 0)
    (void) close (stdin_fd);
  if (stdout_fd != 1)
    (void) close (stdout_fd);

  /* Run the command.  */
  exec_command (argv, envp);
}
#endif /* !AMIGA */
#endif /* !VMS */
#endif /* !WIN32 */

#ifndef _AMIGA
/* Replace the current process with one running the command in ARGV,
   with environment ENVP.  This function does not return.  */

void
exec_command (argv, envp)
     char **argv, **envp;
{
#ifdef VMS
    /* Run the program.  */
    execve (argv[0], argv, envp);
    perror_with_name ("execve: ", argv[0]);
    _exit (EXIT_FAILURE);
#else
  /* Be the user, permanently.  */
  child_access ();

  /* Run the program.  */
  environ = envp;
  execvp (argv[0], argv);

  switch (errno)
    {
    case ENOENT:
      error ("%s: Command not found", argv[0]);
      break;
    case ENOEXEC:
      {
	/* The file is not executable.  Try it as a shell script.  */
	extern char *getenv ();
	char *shell;
	char **new_argv;
	int argc;

	shell = getenv ("SHELL");
	if (shell == 0)
	  shell = default_shell;

	argc = 1;
	while (argv[argc] != 0)
	  ++argc;

	new_argv = (char **) alloca ((1 + argc + 1) * sizeof (char *));
	new_argv[0] = shell;
	new_argv[1] = argv[0];
	while (argc > 0)
	  {
	    new_argv[1 + argc] = argv[argc];
	    --argc;
	  }

	execvp (shell, new_argv);
	if (errno == ENOENT)
	  error ("%s: Shell program not found", shell);
	else
	  perror_with_name ("execvp: ", shell);
	break;
      }

    default:
      perror_with_name ("execvp: ", argv[0]);
      break;
    }

  _exit (127);
#endif /* !VMS */
}
#else /* On Amiga */
void exec_command (argv)
     char **argv;
{
    MyExecute (argv);
}

void clean_tmp (void)
{
    DeleteFile (amiga_bname);
}

#endif /* An Amiga */

#ifndef VMS
/* Figure out the argument list necessary to run LINE as a command.  Try to
   avoid using a shell.  This routine handles only ' quoting, and " quoting
   when no backslash, $ or ` characters are seen in the quotes.  Starting
   quotes may be escaped with a backslash.  If any of the characters in
   sh_chars[] is seen, or any of the builtin commands listed in sh_cmds[]
   is the first word of a line, the shell is used.

   If RESTP is not NULL, *RESTP is set to point to the first newline in LINE.
   If *RESTP is NULL, newlines will be ignored.

   SHELL is the shell to use, or nil to use the default shell.
   IFS is the value of $IFS, or nil (meaning the default).  */

static char **
construct_command_argv_internal (line, restp, shell, ifs)
     char *line, **restp;
     char *shell, *ifs;
{
#ifdef __MSDOS__
  static char sh_chars[] = "\"|<>";
  static char *sh_cmds[] = { "break", "call", "cd", "chcp", "chdir", "cls",
			     "copy", "ctty", "date", "del", "dir", "echo",
			     "erase", "exit", "for", "goto", "if", "if", "md",
			     "mkdir", "path", "pause", "prompt", "rem", "ren",
			     "rename", "set", "shift", "time", "type",
			     "ver", "verify", "vol", ":", 0 };
#else
#ifdef _AMIGA
  static char sh_chars[] = "#;\"|<>()?*$`";
  static char *sh_cmds[] = { "cd", "eval", "if", "delete", "echo", "copy",
			     "rename", "set", "setenv", "date", "makedir",
			     "skip", "else", "endif", "path", "prompt",
			     "unset", "unsetenv", "version",
			     0 };
#else
#ifdef WIN32
  static char sh_chars_dos[] = "\"|<>";
  static char *sh_cmds_dos[] = { "break", "call", "cd", "chcp", "chdir", "cls",
			     "copy", "ctty", "date", "del", "dir", "echo",
			     "erase", "exit", "for", "goto", "if", "if", "md",
			     "mkdir", "path", "pause", "prompt", "rem", "ren",
			     "rename", "set", "shift", "time", "type",
			     "ver", "verify", "vol", ":", 0 };
  static char sh_chars_sh[] = "#;\"*?[]&|<>(){}$`^";
  static char *sh_cmds_sh[] = { "cd", "eval", "exec", "exit", "login",
			     "logout", "set", "umask", "wait", "while", "for",
			     "case", "if", ":", ".", "break", "continue",
			     "export", "read", "readonly", "shift", "times",
			     "trap", "switch", "test", 0 };
  char*  sh_chars;
  char** sh_cmds;
#else  /* WIN32 */
  static char sh_chars[] = "#;\"*?[]&|<>(){}$`^";
  static char *sh_cmds[] = { "cd", "eval", "exec", "exit", "login",
			     "logout", "set", "umask", "wait", "while", "for",
			     "case", "if", ":", ".", "break", "continue",
			     "export", "read", "readonly", "shift", "times",
			     "trap", "switch", 0 };
#endif /* WIN32 */
#endif /* Amiga */
#endif /* MSDOS */
  register int i;
  register char *p;
  register char *ap;
  char *end;
  int instring, word_has_equals, seen_nonequals;
  char **new_argv = 0;
#ifdef WIN32
  int slow_flag = 0;

  if (no_default_sh_exe) {
    sh_cmds = sh_cmds_dos;
    sh_chars = sh_chars_dos;
  } else {
    sh_cmds = sh_cmds_sh;
    sh_chars = sh_chars_sh;
  }
#endif

  if (restp != NULL)
    *restp = NULL;

  /* Make sure not to bother processing an empty line.  */
  while (isblank (*line))
    ++line;
  if (*line == '\0')
    return 0;

  /* See if it is safe to parse commands internally.  */
  if (shell == 0)
    shell = default_shell;
  else if (strcmp (shell, default_shell))
#ifdef WIN32
  {
    char *s1 = _fullpath(NULL, shell, 0);
    char *s2 = _fullpath(NULL, default_shell, 0);

    slow_flag = strcmp((s1 ? s1 : ""), (s2 ? s2 : ""));

    if (s1);
      free(s1);
    if (s2);
      free(s2);
  }
  if (slow_flag)
#endif /* WIN32 */
    goto slow;

  if (ifs != 0)
    for (ap = ifs; *ap != '\0'; ++ap)
      if (*ap != ' ' && *ap != '\t' && *ap != '\n')
	goto slow;

  i = strlen (line) + 1;

  /* More than 1 arg per character is impossible.  */
  new_argv = (char **) xmalloc (i * sizeof (char *));

  /* All the args can fit in a buffer as big as LINE is.   */
  ap = new_argv[0] = (char *) xmalloc (i);
  end = ap + i;

  /* I is how many complete arguments have been found.  */
  i = 0;
  instring = word_has_equals = seen_nonequals = 0;
  for (p = line; *p != '\0'; ++p)
    {
      if (ap > end)
	abort ();

      if (instring)
	{
	string_char:
	  /* Inside a string, just copy any char except a closing quote
	     or a backslash-newline combination.  */
	  if (*p == instring)
	    instring = 0;
	  else if (*p == '\\' && p[1] == '\n')
	    goto swallow_escaped_newline;
	  else if (*p == '\n' && restp != NULL)
	    {
	      /* End of the command line.  */
	      *restp = p;
	      goto end_of_line;
	    }
	  /* Backslash, $, and ` are special inside double quotes.
	     If we see any of those, punt.  */
	  else if (instring == '"' && index ("\\$`", *p) != 0)
	    goto slow;
	  else
	    *ap++ = *p;
	}
      else if (index (sh_chars, *p) != 0)
	/* Not inside a string, but it's a special char.  */
	goto slow;
      else
	/* Not a special char.  */
	switch (*p)
	  {
	  case '=':
	    /* Equals is a special character in leading words before the
	       first word with no equals sign in it.  This is not the case
	       with sh -k, but we never get here when using nonstandard
	       shell flags.  */
	    if (! seen_nonequals)
	      goto slow;
	    word_has_equals = 1;
	    *ap++ = '=';
	    break;

	  case '\\':
	    /* Backslash-newline combinations are eaten.  */
	    if (p[1] == '\n')
	      {
	      swallow_escaped_newline:

		/* Eat the backslash, the newline, and following whitespace,
		   replacing it all with a single space.  */
		p += 2;

		/* If there is a tab after a backslash-newline,
		   remove it from the source line which will be echoed,
		   since it was most likely used to line
		   up the continued line with the previous one.  */
		if (*p == '\t')
		  strcpy (p, p + 1);

		if (instring)
		  goto string_char;
		else
		  {
		    if (ap != new_argv[i])
		      /* Treat this as a space, ending the arg.
			 But if it's at the beginning of the arg, it should
			 just get eaten, rather than becoming an empty arg. */
		      goto end_of_arg;
		    else
		      p = next_token (p) - 1;
		  }
	      }
	    else if (p[1] != '\0')
	      /* Copy and skip the following char.  */
	      *ap++ = *++p;
	    break;

	  case '\'': case_coverage = case_coverage;
	  case '"':
	    instring = *p;
	    break;

	  case '\n':
	    if (restp != NULL)
	      {
		/* End of the command line.  */
		*restp = p;
		goto end_of_line;
	      }
	    else
	      /* Newlines are not special.  */
	      *ap++ = '\n';
	    break;

	  case ' ': case_coverage = case_coverage;
	  case '\t':
	  end_of_arg:
	    /* We have the end of an argument.
	       Terminate the text of the argument.  */
	    *ap++ = '\0';
	    new_argv[++i] = ap;

	    /* Update SEEN_NONEQUALS, which tells us if every word
	       heretofore has contained an `='.  */
	    seen_nonequals |= ! word_has_equals;
	    if (word_has_equals && ! seen_nonequals)
	      /* An `=' in a word before the first
		 word without one is magical.  */
	      goto slow;
	    word_has_equals = 0; /* Prepare for the next word.  */

	    /* If this argument is the command name,
	       see if it is a built-in shell command.
	       If so, have the shell handle it.  */
	    if (i == 1)
	      {
		register int j;
		for (j = 0; sh_cmds[j] != 0; ++j)
		  if (streq (sh_cmds[j], new_argv[0]))
		    goto slow;
	      }

	    /* Ignore multiple whitespace chars.  */
	    p = next_token (p);
	    /* Next iteration should examine the first nonwhite char.  */
	    --p;
	    break;

	  default:
	    *ap++ = *p;
	    break;
	  }
    }
 end_of_line:

  if (instring)
    /* Let the shell deal with an unterminated quote.  */
    goto slow;

  /* Terminate the last argument and the argument list.  */

  *ap = '\0';
  if (new_argv[i][0] != '\0')
    ++i;
  new_argv[i] = 0;

  if (i == 1)
    {
      register int j;
      for (j = 0; sh_cmds[j] != 0; ++j)
	if (streq (sh_cmds[j], new_argv[0]))
	  goto slow;
    }

  if (new_argv[0] == 0)
    /* Line was empty.  */
    return 0;
  else
    return new_argv;

 slow:;
  /* We must use the shell.  */

  if (new_argv != 0)
    {
      /* Free the old argument list we were working on.  */
      free (new_argv[0]);
      free ((void *)new_argv);
    }
#ifdef WIN32
  /*
   * Not eating this whitespace caused things like
   *
   *    sh -c "\n"
   *
   * which gave the shell fits. I think we have to eat
   * whitespace here, but this code should be considered
   * suspicious if things start failing....
   */

  /* Make sure not to bother processing an empty line.  */
  while (isspace (*line))
    ++line;
  if (*line == '\0')
    return 0;
#endif

#if defined(__MSDOS__) || defined(WIN32)
#ifdef WIN32
   /*
    * only come here if no sh.exe command
    */
   if (no_default_sh_exe)
#endif
   {
     FILE *batch;
     dos_batch_file = 1;
     if (dos_bname == 0)
       {
	 dos_bname = tempnam (".", "mk");
	 for (i = 0; dos_bname[i] != '\0'; ++i)
	   if (dos_bname[i] == '/')
	     dos_bname[i] = '\\';
	 dos_bename = (char *) xmalloc (strlen (dos_bname) + 5);
	 strcpy (dos_bename, dos_bname);
	 strcat (dos_bname, ".bat");
	 strcat (dos_bename, ".err");
       }
     batch = fopen (dos_bename, "w"); /* Create a file.  */
     if (batch != NULL)
       fclose (batch);
     batch = fopen (dos_bname, "w");
     fputs ("@echo off\n", batch);
     fputs (line, batch);
     fprintf (batch, "\nif errorlevel 1 del %s\n", dos_bename);
     fclose (batch);
     new_argv = (char **) xmalloc(2 * sizeof(char *));
     new_argv[0] = strdup (dos_bname);
     new_argv[1] = 0;
   }
#endif /* MSDOS. */
#ifdef _AMIGA
   {
     char *ptr;
     char *buffer;
     char *dptr;

     buffer = (char *)xmalloc (strlen (line)+1);

     ptr = line;
     for (dptr=buffer; *ptr; )
     {
	if (*ptr == '\\' && ptr[1] == '\n')
	    ptr += 2;
	else if (*ptr == '@') /* Kludge: multiline commands */
	{
	    ptr += 2;
	    *dptr++ = '\n';
	}
	else
	    *dptr++ = *ptr++;
     }
     *dptr = 0;

     new_argv = (char **) xmalloc(2 * sizeof(char *));
     new_argv[0] = buffer;
     new_argv[1] = 0;
   }
#else	/* Not MSDOS or Amiga  */
#ifdef WIN32
  /*
   * This is technically an else to the above 'if (no_default_sh_exe)',
   * but (IMHO) coding if-else across ifdef is dangerous.
   */
  if (!no_default_sh_exe)
#endif
  {
    /* SHELL may be a multi-word command.  Construct a command line
       "SHELL -c LINE", with all special chars in LINE escaped.
       Then recurse, expanding this command line to get the final
       argument list.  */

    unsigned int shell_len = strlen (shell);
    static char minus_c[] = " -c ";
    unsigned int line_len = strlen (line);

    char *new_line = (char *) alloca (shell_len + (sizeof (minus_c) - 1)
				      + (line_len * 2) + 1);

    ap = new_line;
    bcopy (shell, ap, shell_len);
    ap += shell_len;
    bcopy (minus_c, ap, sizeof (minus_c) - 1);
    ap += sizeof (minus_c) - 1;
    for (p = line; *p != '\0'; ++p)
      {
	if (restp != NULL && *p == '\n')
	  {
	    *restp = p;
	    break;
	  }
	else if (*p == '\\' && p[1] == '\n')
	  {
	    /* Eat the backslash, the newline, and following whitespace,
	       replacing it all with a single space (which is escaped
	       from the shell).  */
	    p += 2;

	    /* If there is a tab after a backslash-newline,
	       remove it from the source line which will be echoed,
	       since it was most likely used to line
	       up the continued line with the previous one.  */
	    if (*p == '\t')
	      strcpy (p, p + 1);

	    p = next_token (p);
	    --p;
	    *ap++ = '\\';
	    *ap++ = ' ';
	    continue;
	  }

	if (*p == '\\' || *p == '\'' || *p == '"'
	    || isspace (*p)
	    || index (sh_chars, *p) != 0)
	  *ap++ = '\\';
	*ap++ = *p;
      }
    *ap = '\0';

    new_argv = construct_command_argv_internal (new_line, (char **) NULL,
						(char *) 0, (char *) 0);
  }
#endif	/* Not MSDOS nor Amiga.  */

  return new_argv;
}

/* Figure out the argument list necessary to run LINE as a command.  Try to
   avoid using a shell.  This routine handles only ' quoting, and " quoting
   when no backslash, $ or ` characters are seen in the quotes.  Starting
   quotes may be escaped with a backslash.  If any of the characters in
   sh_chars[] is seen, or any of the builtin commands listed in sh_cmds[]
   is the first word of a line, the shell is used.

   If RESTP is not NULL, *RESTP is set to point to the first newline in LINE.
   If *RESTP is NULL, newlines will be ignored.

   FILE is the target whose commands these are.  It is used for
   variable expansion for $(SHELL) and $(IFS).  */

char **
construct_command_argv (line, restp, file)
     char *line, **restp;
     struct file *file;
{
  char *shell, *ifs;
  char **argv;

  {
    /* Turn off --warn-undefined-variables while we expand SHELL and IFS.  */
    int save = warn_undefined_variables_flag;
    warn_undefined_variables_flag = 0;

    shell = allocated_variable_expand_for_file ("$(SHELL)", file);
#ifdef WIN32
    /*
     * Convert to forward slashes so that construct_command_argv_internal()
     * is not confused.
     */
    if (shell) {
      char *p = w32ify(shell, 0);
      strcpy(shell, p);
    }
#endif
    ifs = allocated_variable_expand_for_file ("$(IFS)", file);

    warn_undefined_variables_flag = save;
  }

  argv = construct_command_argv_internal (line, restp, shell, ifs);

  free (shell);
  free (ifs);

  return argv;
}
#endif /* !VMS */

#if !defined(HAVE_DUP2) && !defined(_AMIGA)
int
dup2 (old, new)
     int old, new;
{
  int fd;

  (void) close (new);
  fd = dup (old);
  if (fd != new)
    {
      (void) close (fd);
      errno = EMFILE;
      return -1;
    }

  return fd;
}
#endif /* !HAPE_DUP2 && !_AMIGA */






/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         dir.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/


/* Directory hashing for GNU Make.
Copyright (C) 1988, 89, 91, 92, 93, 94, 95, 96 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
#undef stderr
#define stderr stdout

#ifdef	HAVE_DIRENT_H
# include <dirent.h>
#undef stderr
#define stderr stdout
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
#undef stderr
#define stderr stdout
# endif
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
#undef stderr
#define stderr stdout
# endif
# ifdef HAVE_NDIR_H
#  include <ndir.h>
#undef stderr
#define stderr stdout
# endif
# ifdef HAVE_VMSDIR_H
#  include "vmsdir.h"
#undef stderr
#define stderr stdout
# endif /* HAVE_VMSDIR_H */
#endif

/* In GNU systems, <dirent.h> defines this macro for us.  */
#ifdef _D_NAMLEN
#undef NAMLEN
#define NAMLEN(d) _D_NAMLEN(d)
#endif

#if (defined (POSIX) || defined (WIN32)) && !defined (__GNU_LIBRARY__)
/* Posix does not require that the d_ino field be present, and some
   systems do not provide it. */
#define REAL_DIR_ENTRY(dp) 1
#define FAKE_DIR_ENTRY(dp)
#else
#define REAL_DIR_ENTRY(dp) (dp->d_ino != 0)
#define FAKE_DIR_ENTRY(dp) (dp->d_ino = 1)
#endif /* POSIX */

#ifdef __MSDOS__
#include <ctype.h>
#undef stderr
#define stderr stdout
#if (DJGPP > 1)
#include <libc/dosio.h>
#undef stderr
#define stderr stdout
int __opendir_flags = 0;
#endif

static char *
dosify (filename)
     char *filename;
{
  static char dos_filename[14];
  char *df;
  int i;

#if (DJGPP > 1)
  if (_USE_LFN)
    /* Using long file names; do no transformation.  */
    return filename;
#endif
  if (filename == 0)
    return 0;

  if (strpbrk (filename, "\"*+,;<=>?[\\]|") != 0)
    return filename;

  df = dos_filename;

  /* First, transform the name part.  */
  for (i = 0; *filename != '\0' && i < 8 && *filename != '.'; ++i)
    *df++ = tolower (*filename++);

  /* Now skip to the next dot.  */
  while (*filename != '\0' && *filename != '.')
    ++filename;
  if (*filename != '\0')
    {
      *df++ = *filename++;
      for (i = 0; *filename != '\0' && i < 3 && *filename != '.'; ++i)
	*df++ = tolower (*filename++);
    }

  /* Look for more dots.  */
  while (*filename != '\0' && *filename != '.')
    ++filename;
  if (*filename == '.')
    return filename;
  *df = 0;
  return dos_filename;
}
#endif /* __MSDOS__ */

#ifdef WIN32
#include "pathstuff.h"
#undef stderr
#define stderr stdout
#endif

#ifdef _AMIGA
#include <ctype.h>
#undef stderr
#define stderr stdout

static char *
amigafy (filename)
     char *filename;
{
  static char amiga_filename[136];
  char *df;
  int i;

  if (filename == 0)
    return 0;

  df = amiga_filename;

  /* First, transform the name part.  */
  for (i = 0; *filename != '\0'; ++i)
  {
    *df++ = tolower (*filename);
    ++filename;
  }

  *df = 0;

  return amiga_filename;
}
#endif /* _AMIGA */

#ifdef VMS

static int
vms_hash (name)
    char *name;
{
  int h = 0;
  int g;

  while (*name)
    {
      h = (h << 4) + *name++;
      g = h & 0xf0000000;
      if (g)
	{
	  h = h ^ (g >> 24);
	  h = h ^ g;
	}
    }
  return h;
}

/* fake stat entry for a directory */
static int
vmsstat_dir (name, st)
    char *name;
    struct stat *st;
{
  char *s;
  int h;
  DIR *dir;

  dir = opendir (name);
  if (dir == 0)
    return -1;
  closedir (dir);
  s = strchr (name, ':');	/* find device */
  if (s)
    {
      *s++ = 0;
      st->st_dev = (char *)vms_hash (name);
    }
  else
    {
      st->st_dev = 0;
      s = name;
    }
  h = vms_hash (s);
  st->st_ino[0] = h & 0xff;
  st->st_ino[1] = h & 0xff00;
  st->st_ino[2] = h >> 16;

  return 0;
}
#endif /* VMS */

/* Hash table of directories.  */

#ifndef	DIRECTORY_BUCKETS
#define DIRECTORY_BUCKETS 199
#endif

struct directory_contents
  {
    struct directory_contents *next;

    dev_t dev;			/* Device and inode numbers of this dir.  */
#ifdef WIN32
    /*
     * Inode means nothing on WIN32. Even file key information is
     * unreliable because it is random per file open and undefined
     * for remote filesystems. The most unique attribute I can
     * come up with is the fully qualified name of the directory. Beware
     * though, this is also unreliable. I'm open to suggestion on a better
     * way to emulate inode.
     */
    char *path_key;
    int   mtime;        /* controls check for stale directory cache */
#else
#ifdef VMS
    ino_t ino[3];
#else
    ino_t ino;
#endif
#endif /* WIN32 */
    struct dirfile **files;	/* Files in this directory.  */
    DIR *dirstream;		/* Stream reading this directory.  */
  };

/* Table of directory contents hashed by device and inode number.  */
static struct directory_contents *directories_contents[DIRECTORY_BUCKETS];

struct directory
  {
    struct directory *next;

    char *name;			/* Name of the directory.  */

    /* The directory's contents.  This data may be shared by several
       entries in the hash table, which refer to the same directory
       (identified uniquely by `dev' and `ino') under different names.  */
    struct directory_contents *contents;
  };

/* Table of directories hashed by name.  */
static struct directory *table_of_directories[DIRECTORY_BUCKETS];


/* Never have more than this many directories open at once.  */

#define MAX_OPEN_DIRECTORIES 10

static unsigned int open_directories = 0;


/* Hash table of files in each directory.  */

struct dirfile
  {
    struct dirfile *next;
    char *name;			/* Name of the file.  */
    char impossible;		/* This file is impossible.  */
  };

#ifndef	DIRFILE_BUCKETS
#define DIRFILE_BUCKETS 107
#endif

static int dir_contents_file_exists_p PARAMS ((struct directory_contents *dir, char *filename));
static struct directory *find_directory PARAMS ((char *name));

/* Find the directory named NAME and return its `struct directory'.  */

static struct directory *
find_directory (name)
     register char *name;
{
  register unsigned int hash = 0;
  register char *p;
  register struct directory *dir;
#ifdef WIN32
  char* w32_path;
#endif
#ifdef VMS
  if ((*name == '.') && (*(name+1) == 0))
    name = "[]";
  else
    name = vmsify (name,1);
#endif

  for (p = name; *p != '\0'; ++p)
    HASHI (hash, *p);
  hash %= DIRECTORY_BUCKETS;

  for (dir = table_of_directories[hash]; dir != 0; dir = dir->next)
    if (strieq (dir->name, name))
      break;

  if (dir == 0)
    {
      struct stat st;

      /* The directory was not found.  Create a new entry for it.  */

      dir = (struct directory *) xmalloc (sizeof (struct directory));
      dir->next = table_of_directories[hash];
      table_of_directories[hash] = dir;
      dir->name = savestring (name, p - name);

      /* The directory is not in the name hash table.
	 Find its device and inode numbers, and look it up by them.  */

#ifdef VMS
      if (vmsstat_dir (name, &st) < 0)
#else
      if (stat (name, &st) < 0)
#endif
	{
	/* Couldn't stat the directory.  Mark this by
	   setting the `contents' member to a nil pointer.  */
	  dir->contents = 0;
	}
      else
	{
	  /* Search the contents hash table; device and inode are the key.  */

	  struct directory_contents *dc;

#ifdef WIN32
          w32_path = w32ify(name, 1);
          hash = ((unsigned int) st.st_dev << 16) | (unsigned int) st.st_ctime;
#else
#ifdef VMS
	hash = ((unsigned int) st.st_dev << 16)
		| ((unsigned int) st.st_ino[0]
		+ (unsigned int) st.st_ino[1]
		+ (unsigned int) st.st_ino[2]);
#else
	  hash = ((unsigned int) st.st_dev << 16) | (unsigned int) st.st_ino;
#endif
#endif
	  hash %= DIRECTORY_BUCKETS;

	  for (dc = directories_contents[hash]; dc != 0; dc = dc->next)
#ifdef WIN32
            if (!strcmp(dc->path_key, w32_path))
#else
	    if (dc->dev == st.st_dev
#ifdef VMS
		&& dc->ino[0] == st.st_ino[0]
		&& dc->ino[1] == st.st_ino[1]
		&& dc->ino[2] == st.st_ino[2])
#else
		 && dc->ino == st.st_ino)
#endif
#endif /* WIN32 */
	      break;

	  if (dc == 0)
	    {
	      /* Nope; this really is a directory we haven't seen before.  */

	      dc = (struct directory_contents *)
		xmalloc (sizeof (struct directory_contents));

	      /* Enter it in the contents hash table.  */
	      dc->dev = st.st_dev;
#ifdef WIN32
              dc->path_key = strdup(w32_path);
              dc->mtime = st.st_mtime;
#else
#ifdef VMS
	      dc->ino[0] = st.st_ino[0];
	      dc->ino[1] = st.st_ino[1];
	      dc->ino[2] = st.st_ino[2];
#else
	      dc->ino = st.st_ino;
#endif
#endif /* WIN32 */
	      dc->next = directories_contents[hash];
	      directories_contents[hash] = dc;

#if defined (__MSDOS__) && (DJGPP > 1)
	      if (_USE_LFN)
		/* We are using long filenames, so tell opendir not
		   to mess with them.  */
		__opendir_flags = __OPENDIR_PRESERVE_CASE;
#endif

	      dc->dirstream = opendir (name);
	      if (dc->dirstream == 0)
		{
		/* Couldn't open the directory.  Mark this by
		   setting the `files' member to a nil pointer.  */
		  dc->files = 0;
		}
	      else
		{
		  /* Allocate an array of buckets for files and zero it.  */
		  dc->files = (struct dirfile **)
		    xmalloc (sizeof (struct dirfile *) * DIRFILE_BUCKETS);
		  bzero ((char *) dc->files,
			 sizeof (struct dirfile *) * DIRFILE_BUCKETS);

		  /* Keep track of how many directories are open.  */
		  ++open_directories;
		  if (open_directories == MAX_OPEN_DIRECTORIES)
		    /* We have too many directories open already.
		       Read the entire directory and then close it.  */
		    (void) dir_contents_file_exists_p (dc, (char *) 0);
		}
	    }

	  /* Point the name-hashed entry for DIR at its contents data.  */
	  dir->contents = dc;
	}
    }

  return dir;
}

/* Return 1 if the name FILENAME is entered in DIR's hash table.
   FILENAME must contain no slashes.  */

static int
dir_contents_file_exists_p (dir, filename)
     register struct directory_contents *dir;
     register char *filename;
{
  register unsigned int hash;
  register char *p;
  register struct dirfile *df;
  register struct dirent *d;
#ifdef WIN32
  struct stat st;
  int rehash = 0;
#endif

  if (dir == 0 || dir->files == 0)
    {
    /* The directory could not be stat'd or opened.  */
      return 0;
    }
#ifdef __MSDOS__
  filename = dosify (filename);
#endif

#ifdef _AMIGA
  filename = amigafy (filename);
#endif

#ifdef VMS
  filename = vmsify (filename,0);
#endif

  hash = 0;
  if (filename != 0)
    {
      if (*filename == '\0')
	{
	/* Checking if the directory exists.  */
	  return 1;
	}

      for (p = filename; *p != '\0'; ++p)
	HASH (hash, *p);
      hash %= DIRFILE_BUCKETS;

      /* Search the list of hashed files.  */

      for (df = dir->files[hash]; df != 0; df = df->next)
	{
	  if (strieq (df->name, filename))
	    {
	      return !df->impossible;
	    }
	}
    }

  /* The file was not found in the hashed list.
     Try to read the directory further.  */

  if (dir->dirstream == 0)
    {
#ifdef WIN32
      /* Check to see if directory has changed since last read */
      if (dir->path_key &&
          stat(dir->path_key, &st) == 0 &&
          st.st_mtime > dir->mtime) {

        /* reset date stamp to show most recent re-process */
        dir->mtime = st.st_mtime;

        /* make sure directory can still be opened */
        dir->dirstream = opendir(dir->path_key);

        if (dir->dirstream)
          rehash = 1;
        else
          return 0; /* couldn't re-read - fail */
      } else
#endif
    /* The directory has been all read in.  */
      return 0;
    }

  while ((d = readdir (dir->dirstream)) != 0)
    {
      /* Enter the file in the hash table.  */
      register unsigned int newhash = 0;
      unsigned int len;
      register unsigned int i;

      if (!REAL_DIR_ENTRY (d))
	continue;

      len = NAMLEN (d);
      for (i = 0; i < len; ++i)
	HASHI (newhash, d->d_name[i]);
      newhash %= DIRFILE_BUCKETS;
#ifdef WIN32
      /*
       * If re-reading a directory, check that this file isn't already
       * in the cache.
       */
      if (rehash) {
        for (df = dir->files[newhash]; df != 0; df = df->next)
          if (streq(df->name, d->d_name))
            break;
      } else
        df = 0;

      /*
       * If re-reading a directory, don't cache files that have
       * already been discovered.
       */
      if (!df) {
#endif

      df = (struct dirfile *) xmalloc (sizeof (struct dirfile));
      df->next = dir->files[newhash];
      dir->files[newhash] = df;
      df->name = savestring (d->d_name, len);
      df->impossible = 0;
#ifdef WIN32
      }
#endif
      /* Check if the name matches the one we're searching for.  */
      if (filename != 0
	  && newhash == hash && strieq (d->d_name, filename))
	{
	  return 1;
	}
    }

  /* If the directory has been completely read in,
     close the stream and reset the pointer to nil.  */
  if (d == 0)
    {
      --open_directories;
      closedir (dir->dirstream);
      dir->dirstream = 0;
    }
  return 0;
}

/* Return 1 if the name FILENAME in directory DIRNAME
   is entered in the dir hash table.
   FILENAME must contain no slashes.  */

int
dir_file_exists_p (dirname, filename)
     register char *dirname;
     register char *filename;
{
  return dir_contents_file_exists_p (find_directory (dirname)->contents,
				     filename);
}

/* Return 1 if the file named NAME exists.  */

int
file_exists_p (name)
     register char *name;
{
  char *dirend;
  char *dirname;

#ifndef	NO_ARCHIVES
  if (ar_name (name))
    return ar_member_date (name) != (time_t) -1;
#endif

#ifdef VMS
  dirend = rindex (name, ']');
  dirend++;
  if (dirend == (char *)1)
    return dir_file_exists_p ("[]", name);
#else /* !VMS */
  dirend = rindex (name, '/');
#ifdef WIN32
  if (!dirend)
    dirend = rindex(name, '\\');
#endif /* WIN32 */
  if (dirend == 0)
    return dir_file_exists_p (".", name);
  if (dirend == 0)
#ifndef _AMIGA
    return dir_file_exists_p (".", name);
#else /* !VMS && !AMIGA */
    return dir_file_exists_p ("", name);
#endif /* AMIGA */
#endif /* VMS */

  dirname = (char *) alloca (dirend - name + 1);
  bcopy (name, dirname, dirend - name);
  dirname[dirend - name] = '\0';
  return dir_file_exists_p (dirname, dirend + 1);
}

/* Mark FILENAME as `impossible' for `file_impossible_p'.
   This means an attempt has been made to search for FILENAME
   as an intermediate file, and it has failed.  */

void
file_impossible (filename)
     register char *filename;
{
  char *dirend;
  register char *p = filename;
  register unsigned int hash;
  register struct directory *dir;
  register struct dirfile *new;

#ifdef VMS
  dirend = rindex (p, ']');
  dirend++;
  if (dirend == (char *)1)
    dir = find_directory ("[]");
#else
  dirend = rindex (p, '/');
  if (dirend == 0)
#ifdef _AMIGA
    dir = find_directory ("");
#else /* !VMS && !AMIGA */
    dir = find_directory (".");
#endif /* AMIGA */
#endif /* VMS */
  else
    {
      char *dirname = (char *) alloca (dirend - p + 1);
      bcopy (p, dirname, dirend - p);
      dirname[dirend - p] = '\0';
      dir = find_directory (dirname);
      filename = p = dirend + 1;
    }

  for (hash = 0; *p != '\0'; ++p)
    HASHI (hash, *p);
  hash %= DIRFILE_BUCKETS;

  if (dir->contents == 0)
    {
      /* The directory could not be stat'd.  We allocate a contents
	 structure for it, but leave it out of the contents hash table.  */
      dir->contents = (struct directory_contents *)
	xmalloc (sizeof (struct directory_contents));
#ifdef WIN32
      dir->contents->path_key = NULL;
      dir->contents->mtime = 0;
#else  /* WIN32 */
#ifdef VMS
      dir->contents->dev = 0;
      dir->contents->ino[0] = dir->contents->ino[1] =
	dir->contents->ino[2] = 0;
#else
      dir->contents->dev = dir->contents->ino = 0;
#endif
#endif /* WIN32 */
      dir->contents->files = 0;
      dir->contents->dirstream = 0;
    }

  if (dir->contents->files == 0)
    {
      /* The directory was not opened; we must allocate the hash buckets.  */
      dir->contents->files = (struct dirfile **)
	xmalloc (sizeof (struct dirfile) * DIRFILE_BUCKETS);
      bzero ((char *) dir->contents->files,
	     sizeof (struct dirfile) * DIRFILE_BUCKETS);
    }

  /* Make a new entry and put it in the table.  */

  new = (struct dirfile *) xmalloc (sizeof (struct dirfile));
  new->next = dir->contents->files[hash];
  dir->contents->files[hash] = new;
  new->name = savestring (filename, strlen (filename));
  new->impossible = 1;
}

/* Return nonzero if FILENAME has been marked impossible.  */

int
file_impossible_p (filename)
     char *filename;
{
  char *dirend;
  register char *p = filename;
  register unsigned int hash;
  register struct directory_contents *dir;
  register struct dirfile *next;

#ifdef VMS
  dirend = rindex (filename, ']');
  if (dirend == 0)
    dir = find_directory ("[]")->contents;
#else
  dirend = rindex (filename, '/');
#ifdef WIN32
  if (!dirend)
    dirend = rindex (filename, '\\');
#endif /* WIN32 */
  if (dirend == 0)
#ifdef _AMIGA
    dir = find_directory ("")->contents;
#else /* !VMS && !AMIGA */
    dir = find_directory (".")->contents;
#endif /* AMIGA */
#endif /* VMS */
  else
    {
      char *dirname = (char *) alloca (dirend - filename + 1);
      bcopy (p, dirname, dirend - p);
      dirname[dirend - p] = '\0';
      dir = find_directory (dirname)->contents;
      p = filename = dirend + 1;
    }

  if (dir == 0 || dir->files == 0)
    /* There are no files entered for this directory.  */
    return 0;

#ifdef __MSDOS__
  p = filename = dosify (p);
#endif
#ifdef _AMIGA
  p = filename = amigafy (p);
#endif
#ifdef VMS
  p = filename = vmsify (p, 1);
#endif

  for (hash = 0; *p != '\0'; ++p)
    HASH (hash, *p);
  hash %= DIRFILE_BUCKETS;

  for (next = dir->files[hash]; next != 0; next = next->next)
    if (strieq (filename, next->name))
      return next->impossible;

  return 0;
}

/* Return the already allocated name in the
   directory hash table that matches DIR.  */

char *
dir_name (dir)
     char *dir;
{
  return find_directory (dir)->name;
}

/* Print the data base of directories.  */

void
print_dir_data_base ()
{
  register unsigned int i, dirs, files, impossible;
  register struct directory *dir;

  puts ("\n# Directories\n");

  dirs = files = impossible = 0;
  for (i = 0; i < DIRECTORY_BUCKETS; ++i)
    for (dir = table_of_directories[i]; dir != 0; dir = dir->next)
      {
	++dirs;
	if (dir->contents == 0)
	  printf ("# %s: could not be stat'd.\n", dir->name);
	else if (dir->contents->files == 0)
#ifdef WIN32
          printf ("# %s (key %s, mtime %d): could not be opened.\n",
                  dir->name, dir->contents->path_key,dir->contents->mtime);
#else  /* WIN32 */
#ifdef VMS
	  printf ("# %s (device %d, inode [%d,%d,%d]): could not be opened.\n",
		  dir->name, dir->contents->dev,
		  dir->contents->ino[0], dir->contents->ino[1],
		  dir->contents->ino[2]);
#else
	  printf ("# %s (device %ld, inode %ld): could not be opened.\n",
		  dir->name, (long int) dir->contents->dev,
		  (long int) dir->contents->ino);
#endif
#endif /* WIN32 */
	else
	  {
	    register unsigned int f = 0, im = 0;
	    register unsigned int j;
	    register struct dirfile *df;
	    for (j = 0; j < DIRFILE_BUCKETS; ++j)
	      for (df = dir->contents->files[j]; df != 0; df = df->next)
		if (df->impossible)
		  ++im;
		else
		  ++f;
#ifdef WIN32
            printf ("# %s (key %s, mtime %d): ",
                    dir->name, dir->contents->path_key, dir->contents->mtime);
#else  /* WIN32 */
#ifdef VMS
	    printf ("# %s (device %d, inode [%d,%d,%d]): ",
		    dir->name, dir->contents->dev,
			dir->contents->ino[0], dir->contents->ino[1],
			dir->contents->ino[2]);
#else
	    printf ("# %s (device %d, inode %d): ",
		    dir->name, dir->contents->dev, dir->contents->ino);
#endif
#endif /* WIN32 */
	    if (f == 0)
	      fputs ("No", stdout);
	    else
	      printf ("%u", f);
	    fputs (" files, ", stdout);
	    if (im == 0)
	      fputs ("no", stdout);
	    else
	      printf ("%u", im);
	    fputs (" impossibilities", stdout);
	    if (dir->contents->dirstream == 0)
	      puts (".");
	    else
	      puts (" so far.");
	    files += f;
	    impossible += im;
	  }
      }

  fputs ("\n# ", stdout);
  if (files == 0)
    fputs ("No", stdout);
  else
    printf ("%u", files);
  fputs (" files, ", stdout);
  if (impossible == 0)
    fputs ("no", stdout);
  else
    printf ("%u", impossible);
  printf (" impossibilities in %u directories.\n", dirs);
}

/* Hooks for globbing.  */

#include <glob.h>
#undef stderr
#define stderr stdout

/* Structure describing state of iterating through a directory hash table.  */

struct dirstream
  {
    struct directory_contents *contents; /* The directory being read.  */

    unsigned int bucket;	/* Current hash bucket.  */
    struct dirfile *elt;	/* Current elt in bucket.  */
  };

/* Forward declarations.  */
static __ptr_t open_dirstream PARAMS ((const char *));
static struct dirent *read_dirstream PARAMS ((__ptr_t));

static __ptr_t
open_dirstream (directory)
     const char *directory;
{
  struct dirstream *new;
  struct directory *dir = find_directory ((char *)directory);

  if (dir->contents == 0 || dir->contents->files == 0)
    /* DIR->contents is nil if the directory could not be stat'd.
       DIR->contents->files is nil if it could not be opened.  */
    return 0;

  /* Read all the contents of the directory now.  There is no benefit
     in being lazy, since glob will want to see every file anyway.  */

  (void) dir_contents_file_exists_p (dir->contents, (char *) 0);

  new = (struct dirstream *) xmalloc (sizeof (struct dirstream));
  new->contents = dir->contents;
  new->bucket = 0;
  new->elt = new->contents->files[0];

  return (__ptr_t) new;
}

static struct dirent *
read_dirstream (stream)
     __ptr_t stream;
{
  struct dirstream *const ds = (struct dirstream *) stream;
  register struct dirfile *df;
  static char *buf;
  static unsigned int bufsz;

  while (ds->bucket < DIRFILE_BUCKETS)
    {
      while ((df = ds->elt) != 0)
	{
	  ds->elt = df->next;
	  if (!df->impossible)
	    {
	      /* The glob interface wants a `struct dirent',
		 so mock one up.  */
	      struct dirent *d;
	      unsigned int len = strlen (df->name) + 1;
	      if (sizeof *d - sizeof d->d_name + len > bufsz)
		{
		  if (buf != 0)
		    free (buf);
		  bufsz *= 2;
		  if (sizeof *d - sizeof d->d_name + len > bufsz)
		    bufsz = sizeof *d - sizeof d->d_name + len;
		  buf = xmalloc (bufsz);
		}
	      d = (struct dirent *) buf;
	      FAKE_DIR_ENTRY (d);
#ifdef _DIRENT_HAVE_D_NAMLEN
	      d->d_namlen = len - 1;
#endif
	      memcpy (d->d_name, df->name, len);
	      return d;
	    }
	}
      if (++ds->bucket == DIRFILE_BUCKETS)
	break;
      ds->elt = ds->contents->files[ds->bucket];
    }

  return 0;
}

void
dir_setup_glob (gl)
     glob_t *gl;
{
  extern int stat ();

  /* Bogus sunos4 compiler complains (!) about & before functions.  */
  gl->gl_opendir = open_dirstream;
  gl->gl_readdir = read_dirstream;
  gl->gl_closedir = free;
  gl->gl_stat = stat;
  /* We don't bother setting gl_lstat, since glob never calls it.
     The slot is only there for compatibility with 4.4 BSD.  */
}




/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         file.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Target file hash table management for GNU Make.
Copyright (C) 1988,89,90,91,92,93,94,95,96 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "dep.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
#include "job.h"
/* #include "commands.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "variable.h"  <- modification by J.Ruthruff, 7/20 */
#include <assert.h>
#undef stderr
#define stderr stdout

/* Hash table of files the makefile knows how to make.  */

#ifndef	FILE_BUCKETS
#define FILE_BUCKETS	1007
#endif
static struct file *files[FILE_BUCKETS];

/* Number of files with the `intermediate' flag set.  */

unsigned int num_intermediates = 0;


/* Access the hash table of all file records.
   lookup_file  given a name, return the struct file * for that name,
           or nil if there is none.
   enter_file   similar, but create one if there is none.  */

struct file *
lookup_file (name)
     char *name;
{
  register struct file *f;
  register char *n;
  register unsigned int hashval;

  if (*name == '\0')
    abort ();

  /* This is also done in parse_file_seq, so this is redundant
     for names read from makefiles.  It is here for names passed
     on the command line.  */
#ifdef VMS
  while (name[0] == '[' && name[1] == ']' && name[2] != '\0')
      name += 2;
#endif
  while (name[0] == '.' && name[1] == '/' && name[2] != '\0')
    {
      name += 2;
      while (*name == '/')
	/* Skip following slashes: ".//foo" is "foo", not "/foo".  */
	++name;
    }

  if (*name == '\0')
    /* It was all slashes after a dot.  */
#ifdef VMS
    name = "[]";
#else
#ifdef _AMIGA
    name = "";
#else
    name = "./";
#endif /* AMIGA */
#endif /* VMS */

  hashval = 0;
  for (n = name; *n != '\0'; ++n)
    HASHI (hashval, *n);
  hashval %= FILE_BUCKETS;

  for (f = files[hashval]; f != 0; f = f->next)
    {
      if (strieq (f->name, name))
	{
	  return f;
	}
    }
  return 0;
}

struct file *
enter_file (name)
     char *name;
{
  register struct file *f, *new;
  register char *n;
  register unsigned int hashval;
#ifdef VMS
  char *lname, *ln;
#endif

  if (*name == '\0')
    abort ();

#ifdef VMS
  lname = (char *)malloc (strlen (name) + 1);
  for (n = name, ln = lname; *n != '\0'; ++n, ++ln)
    {
      if (isupper(*n))
	*ln = tolower(*n);
      else
	*ln = *n;
    }
  *ln = 0;
  name = lname;
#endif

  hashval = 0;
  for (n = name; *n != '\0'; ++n)
    HASHI (hashval, *n);
  hashval %= FILE_BUCKETS;

  for (f = files[hashval]; f != 0; f = f->next)
    if (strieq (f->name, name))
      break;

  if (f != 0 && !f->double_colon)
    {
#ifdef VMS
      free(lname);
#endif
      return f;
    }

  new = (struct file *) xmalloc (sizeof (struct file));
  bzero ((char *) new, sizeof (struct file));
  new->name = name;
  new->update_status = -1;

  if (f == 0)
    {
      /* This is a completely new file.  */
      new->next = files[hashval];
      files[hashval] = new;
    }
  else
    {
      /* There is already a double-colon entry for this file.  */
      new->double_colon = f;
      while (f->prev != 0)
	f = f->prev;
      f->prev = new;
    }

  return new;
}

/* Rename FILE to NAME.  This is not as simple as resetting
   the `name' member, since it must be put in a new hash bucket,
   and possibly merged with an existing file called NAME.  */

void
rename_file (file, name)
     register struct file *file;
     char *name;
{
  char *oldname = file->name;
  register unsigned int oldhash;
  register char *n;

  while (file->renamed != 0)
    file = file->renamed;

  /* Find the hash values of the old and new names.  */

  oldhash = 0;
  for (n = oldname; *n != '\0'; ++n)
    HASHI (oldhash, *n);

  file_hash_enter (file, name, oldhash, file->name);
}

void
file_hash_enter (file, name, oldhash, oldname)
     register struct file *file;
     char *name;
     unsigned int oldhash;
     char *oldname;
{
  unsigned int oldbucket = oldhash % FILE_BUCKETS;
  register unsigned int newhash, newbucket;
  struct file *oldfile;
  register char *n;
  register struct file *f;

  newhash = 0;
  for (n = name; *n != '\0'; ++n)
    HASHI (newhash, *n);
  newbucket = newhash % FILE_BUCKETS;

  /* Look for an existing file under the new name.  */

  for (oldfile = files[newbucket]; oldfile != 0; oldfile = oldfile->next)
    if (strieq (oldfile->name, name))
      break;

  if (oldhash != 0 && (newbucket != oldbucket || oldfile != 0))
    {
      /* Remove FILE from its hash bucket.  */

      struct file *lastf = 0;

      for (f = files[oldbucket]; f != file; f = f->next)
	lastf = f;

      if (lastf == 0)
	files[oldbucket] = f->next;
      else
	lastf->next = f->next;
    }

  /* Give FILE its new name.  */

  file->name = name;
  for (f = file->double_colon; f != 0; f = f->prev)
    f->name = name;

  if (oldfile == 0)
    {
      /* There is no existing file with the new name.  */

      if (newbucket != oldbucket)
	{
	  /* Put FILE in its new hash bucket.  */
	  file->next = files[newbucket];
	  files[newbucket] = file;
	}
    }
  else
    {
      /* There is an existing file with the new name.
	 We must merge FILE into the existing file.  */

      register struct dep *d;

      if (file->cmds != 0)
	{
	  if (oldfile->cmds == 0)
	    oldfile->cmds = file->cmds;
	  else if (file->cmds != oldfile->cmds)
	    {
	      /* We have two sets of commands.  We will go with the
		 one given in the rule explicitly mentioning this name,
		 but give a message to let the user know what's going on.  */
	      if (oldfile->cmds->filename != 0)
		makefile_error (file->cmds->filename, file->cmds->lineno,
				"Commands were specified for \
file `%s' at %s:%u,",
				oldname, oldfile->cmds->filename,
				oldfile->cmds->lineno);
	      else
		makefile_error (file->cmds->filename, file->cmds->lineno,
				"Commands for file `%s' were found by \
implicit rule search,",
				oldname);
	      makefile_error (file->cmds->filename, file->cmds->lineno,
			      "but `%s' is now considered the same file \
as `%s'.",
			      oldname, name);
	      makefile_error (file->cmds->filename, file->cmds->lineno,
			      "Commands for `%s' will be ignored \
in favor of those for `%s'.",
			      name, oldname);
	    }
	}

      /* Merge the dependencies of the two files.  */

      d = oldfile->deps;
      if (d == 0)
	oldfile->deps = file->deps;
      else
	{
	  while (d->next != 0)
	    d = d->next;
	  d->next = file->deps;
	}

      merge_variable_set_lists (&oldfile->variables, file->variables);

      if (oldfile->double_colon && file->is_target && !file->double_colon)
	fatal ("can't rename single-colon `%s' to double-colon `%s'",
	       oldname, name);
      if (!oldfile->double_colon  && file->double_colon)
	{
	  if (oldfile->is_target)
	    fatal ("can't rename double-colon `%s' to single-colon `%s'",
		   oldname, name);
	  else
	    oldfile->double_colon = file->double_colon;
	}

      if (file->last_mtime > oldfile->last_mtime)
	/* %%% Kludge so -W wins on a file that gets vpathized.  */
	oldfile->last_mtime = file->last_mtime;

#define MERGE(field) oldfile->field |= file->field
      MERGE (precious);
      MERGE (tried_implicit);
      MERGE (updating);
      MERGE (updated);
      MERGE (is_target);
      MERGE (cmd_target);
      MERGE (phony);
#undef MERGE

      file->renamed = oldfile;
    }
}

/* Remove all nonprecious intermediate files.
   If SIG is nonzero, this was caused by a fatal signal,
   meaning that a different message will be printed, and
   the message will go to stderr rather than stdout.  */

void
remove_intermediates (sig)
     int sig;
{
  register int i;
  register struct file *f;
  char doneany;

  if (question_flag || touch_flag)
    return;
  if (sig && just_print_flag)
    return;

  doneany = 0;
  for (i = 0; i < FILE_BUCKETS; ++i)
    for (f = files[i]; f != 0; f = f->next)
      if (f->intermediate && (f->dontcare || !f->precious)
	  && !f->secondary)
	{
	  int status;
	  if (f->update_status == -1)
	    /* If nothing would have created this file yet,
	       don't print an "rm" command for it.  */
	    continue;
	  else if (just_print_flag)
	    status = 0;
	  else
	    {
	      status = unlink (f->name);
	      if (status < 0 && errno == ENOENT)
		continue;
	    }
	  if (!f->dontcare)
	    {
	      if (sig)
		error ("*** Deleting intermediate file `%s'", f->name);
	      else if (!silent_flag)
		{
		  if (! doneany)
		    {
		      fputs ("rm ", stdout);
		      doneany = 1;
		    }
		  else
		    putchar (' ');
		  fputs (f->name, stdout);
		  fflush (stdout);
		}
	      if (status < 0)
		perror_with_name ("unlink: ", f->name);
	    }
	}

  if (doneany && !sig)
    {
      putchar ('\n');
      fflush (stdout);
    }
}

/* For each dependency of each file, make the `struct dep' point
   at the appropriate `struct file' (which may have to be created).

   Also mark the files depended on by .PRECIOUS, .PHONY, .SILENT,
   and various other special targets.  */

void
snap_deps ()
{
  register struct file *f, *f2;
  register struct dep *d;
  register int i;

  /* Enter each dependency name as a file.  */
  for (i = 0; i < FILE_BUCKETS; ++i)
    for (f = files[i]; f != 0; f = f->next)
      for (f2 = f; f2 != 0; f2 = f2->prev)
	for (d = f2->deps; d != 0; d = d->next)
	  if (d->name != 0)
	    {
	      d->file = lookup_file (d->name);
	      if (d->file == 0)
		d->file = enter_file (d->name);
	      else
		free (d->name);
	      d->name = 0;
	    }

  for (f = lookup_file (".PRECIOUS"); f != 0; f = f->prev)
    for (d = f->deps; d != 0; d = d->next)
      for (f2 = d->file; f2 != 0; f2 = f2->prev)
	f2->precious = 1;

  for (f = lookup_file (".PHONY"); f != 0; f = f->prev)
    for (d = f->deps; d != 0; d = d->next)
      for (f2 = d->file; f2 != 0; f2 = f2->prev)
	{
	  /* Mark this file as phony and nonexistent.  */
	  f2->phony = 1;
	  f2->last_mtime = (time_t) -1;
	}

  for (f = lookup_file (".INTERMEDIATE"); f != 0; f = f->prev)
    {
      /* .INTERMEDIATE with deps listed
	 marks those deps as intermediate files.  */
      for (d = f->deps; d != 0; d = d->next)
	for (f2 = d->file; f2 != 0; f2 = f2->prev)
	  f2->intermediate = 1;
      /* .INTERMEDIATE with no deps does nothing.
	 Marking all files as intermediates is useless
	 since the goal targets would be deleted after they are built.  */
    }

  for (f = lookup_file (".SECONDARY"); f != 0; f = f->prev)
    {
      /* .SECONDARY with deps listed
	 marks those deps as intermediate files
	 in that they don't get rebuilt if not actually needed;
	 but unlike real intermediate files,
	 these are not deleted after make finishes.  */
      if (f->deps)
	{
	  for (d = f->deps; d != 0; d = d->next)
	    for (f2 = d->file; f2 != 0; f2 = f2->prev)
	      f2->intermediate = f2->secondary = 1;
	}
      /* .SECONDARY with no deps listed marks *all* files that way.  */
      else
	{
	  int i;
	  for (i = 0; i < FILE_BUCKETS; i++)
	    for (f2 = files[i]; f2; f2= f2->next)
	      f2->intermediate = f2->secondary = 1;
	}
    }

  f = lookup_file (".EXPORT_ALL_VARIABLES");
  if (f != 0 && f->is_target)
    export_all_variables = 1;

  f = lookup_file (".IGNORE");
  if (f != 0 && f->is_target)
    {
      if (f->deps == 0)
	ignore_errors_flag = 1;
      else
	for (d = f->deps; d != 0; d = d->next)
	  for (f2 = d->file; f2 != 0; f2 = f2->prev)
	    f2->command_flags |= COMMANDS_NOERROR;
    }

  f = lookup_file (".SILENT");
  if (f != 0 && f->is_target)
    {
      if (f->deps == 0)
	silent_flag = 1;
      else
	for (d = f->deps; d != 0; d = d->next)
	  for (f2 = d->file; f2 != 0; f2 = f2->prev)
	    f2->command_flags |= COMMANDS_SILENT;
    }

  f = lookup_file (".POSIX");
  if (f != 0 && f->is_target)
    posix_pedantic = 1;
}

/* Set the `command_state' member of FILE and all its `also_make's.  */

void
set_command_state (file, state)
     struct file *file;
     int state;
{
  struct dep *d;

  file->command_state = state;

  for (d = file->also_make; d != 0; d = d->next)
    d->file->command_state = state;
}

/* Print the data base of files.  */

static void
print_file (f)
     struct file *f;
{
  register struct dep *d;
#ifdef VMS
  extern char *cvt_time PARAMS ((unsigned long));
#endif
  putchar ('\n');
  if (!f->is_target)
    puts ("# Not a target:");
  printf ("%s:%s", f->name, f->double_colon ? ":" : "");

  for (d = f->deps; d != 0; d = d->next)
    printf (" %s", dep_name (d));
  putchar ('\n');

  if (f->precious)
    puts ("#  Precious file (dependency of .PRECIOUS).");
  if (f->phony)
    puts ("#  Phony target (dependency of .PHONY).");
  if (f->cmd_target)
    puts ("#  Command-line target.");
  if (f->dontcare)
    puts ("#  A default or MAKEFILES makefile.");
  printf ("#  Implicit rule search has%s been done.\n",
	  f->tried_implicit ? "" : " not");
  if (f->stem != 0)
    printf ("#  Implicit/static pattern stem: `%s'\n", f->stem);
  if (f->intermediate)
    puts ("#  File is an intermediate dependency.");
  if (f->also_make != 0)
    {
      fputs ("#  Also makes:", stdout);
      for (d = f->also_make; d != 0; d = d->next)
	printf (" %s", dep_name (d));
      putchar ('\n');
    }
  if (f->last_mtime == (time_t) 0)
    puts ("#  Modification time never checked.");
  else if (f->last_mtime == (time_t) -1)
    puts ("#  File does not exist.");
  else
#ifdef VMS
    printf ("# Last modified Sun Jan 01 2000 00:00:00");
   /*  printf ("#  Last modified %.24s (%0lx)\n",
	    cvt_time(f->last_mtime), (unsigned long) f->last_mtime); */
#else
    printf ("# Last modified Sun Jan 01 2000 00:00:00");
   /*  printf ("#  Last modified %.24s (%ld)\n",
	    ctime (&f->last_mtime), (long int) f->last_mtime); */
#endif
  printf ("#  File has%s been updated.\n",
	  f->updated ? "" : " not");
  switch (f->command_state)
    {
    case cs_running:
      puts ("#  Commands currently running (THIS IS A BUG).");
      break;
    case cs_deps_running:
      puts ("#  Dependencies commands running (THIS IS A BUG).");
      break;
    case cs_not_started: case_coverage = case_coverage;
    case cs_finished:
      switch (f->update_status)
	{
	case -1:
	  break;
	case 0:
	  puts ("#  Successfully updated.");
	  break;
	case 1:
	  assert (question_flag);
	  puts ("#  Needs to be updated (-q is set).");
	  break;
	case 2:
	  puts ("#  Failed to be updated.");
	  break;
	default:
	  puts ("#  Invalid value in `update_status' member!");
	  fflush (stdout);
	  fflush (stderr);
	  abort ();
	}
      break;
    default:
      puts ("#  Invalid value in `command_state' member!");
      fflush (stdout);
      fflush (stderr);
      abort ();
    }

  if (f->variables != 0)
    print_file_variables (f);

  if (f->cmds != 0)
    print_commands (f->cmds);
}

void
print_file_data_base ()
{
  register unsigned int i, nfiles, per_bucket;
  register struct file *file;

  puts ("\n# Files");

  per_bucket = nfiles = 0;
  for (i = 0; i < FILE_BUCKETS; ++i)
    {
      register unsigned int this_bucket = 0;

      for (file = files[i]; file != 0; file = file->next)
	{
	  register struct file *f;

	  ++this_bucket;

	  for (f = file; f != 0; f = f->prev)
	    print_file (f);
	}

      nfiles += this_bucket;
      if (this_bucket > per_bucket)
	per_bucket = this_bucket;
    }

  if (nfiles == 0)
    puts ("\n# No files.");
  else
    {
      printf ("\n# %u files in %u hash buckets.\n", nfiles, FILE_BUCKETS);
#ifndef	NO_FLOAT
      printf ("# average %.1f files per bucket, max %u files in one bucket.\n",
	      ((double) nfiles) / ((double) FILE_BUCKETS) * 100.0, per_bucket);
#endif
    }
}

/* EOF */




/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         misc.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Miscellaneous generic support functions for GNU Make.
Copyright (C) 1988, 89, 90, 91, 92, 93, 94, 1995 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "dep.h"  <- modification by J.Ruthruff, 7/20 */
#undef stderr
#define stderr stdout

/* Compare strings *S1 and *S2.
   Return negative if the first is less, positive if it is greater,
   zero if they are equal.  */

int
alpha_compare (s1, s2)
     char **s1, **s2;
{
  if (**s1 != **s2)
    return **s1 - **s2;
  return strcmp (*s1, *s2);
}

/* Discard each backslash-newline combination from LINE.
   Backslash-backslash-newline combinations become backslash-newlines.
   This is done by copying the text at LINE into itself.  */

void
collapse_continuations (line)
     char *line;
{
  register char *in, *out, *p;
  register int backslash;
  register unsigned int bs_write;

  in = index (line, '\n');
  if (in == 0)
    return;

  out = in;
  while (out > line && out[-1] == '\\')
    --out;

  while (*in != '\0')
    {
      /* BS_WRITE gets the number of quoted backslashes at
	 the end just before IN, and BACKSLASH gets nonzero
	 if the next character is quoted.  */
      backslash = 0;
      bs_write = 0;
      for (p = in - 1; p >= line && *p == '\\'; --p)
	{
	  if (backslash)
	    ++bs_write;
	  backslash = !backslash;

	  /* It should be impossible to go back this far without exiting,
	     but if we do, we can't get the right answer.  */
	  if (in == out - 1)
	    abort ();
	}

      /* Output the appropriate number of backslashes.  */
      while (bs_write-- > 0)
	*out++ = '\\';

      /* Skip the newline.  */
      ++in;

      /* If the newline is quoted, discard following whitespace
	 and any preceding whitespace; leave just one space.  */
      if (backslash)
	{
	  in = next_token (in);
	  while (out > line && isblank (out[-1]))
	    --out;
	  *out++ = ' ';
	}
      else
	/* If the newline isn't quoted, put it in the output.  */
	*out++ = '\n';

      /* Now copy the following line to the output.
	 Stop when we find backslashes followed by a newline.  */
      while (*in != '\0')
	if (*in == '\\')
	  {
	    p = in + 1;
	    while (*p == '\\')
	      ++p;
	    if (*p == '\n')
	      {
		in = p;
		break;
	      }
	    while (in < p)
	      *out++ = *in++;
	  }
	else
	  *out++ = *in++;
    }

  *out = '\0';
}


/* Remove comments from LINE.
   This is done by copying the text at LINE onto itself.  */

void
remove_comments (line)
     char *line;
{
  char *comment;

  comment = find_char_unquote (line, "#", 0);

  if (comment != 0)
    /* Cut off the line at the #.  */
    *comment = '\0';
}

/* Print N spaces (used by DEBUGPR for target-depth).  */

void
print_spaces (n)
     register unsigned int n;
{
  while (n-- > 0)
    putchar (' ');
}


/* Return a newly-allocated string whose contents
   concatenate those of s1, s2, s3.  */

char *
concat (s1, s2, s3)
     register char *s1, *s2, *s3;
{
  register unsigned int len1, len2, len3;
  register char *result;

  len1 = *s1 != '\0' ? strlen (s1) : 0;
  len2 = *s2 != '\0' ? strlen (s2) : 0;
  len3 = *s3 != '\0' ? strlen (s3) : 0;

  result = (char *) xmalloc (len1 + len2 + len3 + 1);

  if (*s1 != '\0')
    bcopy (s1, result, len1);
  if (*s2 != '\0')
    bcopy (s2, result + len1, len2);
  if (*s3 != '\0')
    bcopy (s3, result + len1 + len2, len3);
  *(result + len1 + len2 + len3) = '\0';

  return result;
}

/* Print a message on stdout.  */

void
message (prefix, s1, s2, s3, s4, s5, s6)
     int prefix;
     char *s1, *s2, *s3, *s4, *s5, *s6;
{
  log_working_directory (1);

  if (s1 != 0)
    {
      if (prefix)
	{
	  if (makelevel == 0)
	    printf ("%s: ", program);
	  else
	    printf ("%s[%u]: ", program, makelevel);
	}
      printf (s1, s2, s3, s4, s5, s6);
      putchar ('\n');
    }

  fflush (stdout);
}

/* Print an error message and exit.  */

/* VARARGS1 */
void
fatal (s1, s2, s3, s4, s5, s6)
     char *s1, *s2, *s3, *s4, *s5, *s6;
{
  log_working_directory (1);

  if (makelevel == 0)
    fprintf (stderr, "%s: *** ", program);
  else
    fprintf (stderr, "%s[%u]: *** ", program, makelevel);
  fprintf (stderr, s1, s2, s3, s4, s5, s6);
  fputs (".  Stop.\n", stderr);

  die (2);
}

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */

/* VARARGS1 */

void
error (s1, s2, s3, s4, s5, s6)
     char *s1, *s2, *s3, *s4, *s5, *s6;
{
  log_working_directory (1);

  if (makelevel == 0)
    fprintf (stderr, "%s: ", program);
  else
    fprintf (stderr, "%s[%u]: ", program, makelevel);
  fprintf (stderr, s1, s2, s3, s4, s5, s6);
  putc ('\n', stderr);
  fflush (stderr);
}

void
makefile_error (file, lineno, s1, s2, s3, s4, s5, s6)
     char *file;
     unsigned int lineno;
     char *s1, *s2, *s3, *s4, *s5, *s6;
{
  log_working_directory (1);

  fprintf (stderr, "%s:%u: ", file, lineno);
  fprintf (stderr, s1, s2, s3, s4, s5, s6);
  putc ('\n', stderr);
  fflush (stderr);
}

void
makefile_fatal (file, lineno, s1, s2, s3, s4, s5, s6)
     char *file;
     unsigned int lineno;
     char *s1, *s2, *s3, *s4, *s5, *s6;
{
  log_working_directory (1);

  fprintf (stderr, "%s:%u: *** ", file, lineno);
  fprintf (stderr, s1, s2, s3, s4, s5, s6);
  fputs (".  Stop.\n", stderr);

  die (2);
}

#ifndef HAVE_STRERROR

#undef	strerror

char *
strerror (errnum)
     int errnum;
{
  extern int errno, sys_nerr;
#ifndef __DECC
  extern char *sys_errlist[];
#endif
  static char buf[] = "Unknown error 12345678901234567890";

  if (errno < sys_nerr)
    return sys_errlist[errnum];

  sprintf (buf, "Unknown error %d", errnum);
  return buf;
}
#endif

/* Print an error message from errno.  */

void
perror_with_name (str, name)
     char *str, *name;
{
  error ("%s%s: %s", str, name, strerror (errno));
}

/* Print an error message from errno and exit.  */

void
pfatal_with_name (name)
     char *name;
{
  fatal ("%s: %s", name, strerror (errno));

  /* NOTREACHED */
}

/* Like malloc but get fatal error if memory is exhausted.  */

#undef xmalloc
#undef xrealloc

char *
xmalloc (size)
     unsigned int size;
{
  char *result = (char *) malloc (size);
  if (result == 0)
    fatal ("virtual memory exhausted");
  return result;
}


char *
xrealloc (ptr, size)
     char *ptr;
     unsigned int size;
{
  char *result = (char *) realloc (ptr, size);
  if (result == 0)
    fatal ("virtual memory exhausted");
  return result;
}

char *
savestring (str, length)
     char *str;
     unsigned int length;
{
  register char *out = (char *) xmalloc (length + 1);
  if (length > 0)
    bcopy (str, out, length);
  out[length] = '\0';
  return out;
}

/* Search string BIG (length BLEN) for an occurrence of
   string SMALL (length SLEN).  Return a pointer to the
   beginning of the first occurrence, or return nil if none found.  */

char *
sindex (big, blen, small, slen)
     char *big;
     unsigned int blen;
     char *small;
     unsigned int slen;
{
  register unsigned int b;

  if (blen < 1)
    blen = strlen (big);
  if (slen < 1)
    slen = strlen (small);

  for (b = 0; b < blen; ++b)
    if (big[b] == *small && !strncmp (&big[b + 1], small + 1, slen - 1))
      return (&big[b]);

  return 0;
}

/* Limited INDEX:
   Search through the string STRING, which ends at LIMIT, for the character C.
   Returns a pointer to the first occurrence, or nil if none is found.
   Like INDEX except that the string searched ends where specified
   instead of at the first null.  */

char *
lindex (s, limit, c)
     register char *s, *limit;
     int c;
{
  while (s < limit)
    if (*s++ == c)
      return s - 1;

  return 0;
}

/* Return the address of the first whitespace or null in the string S.  */

char *
end_of_token (s)
     char *s;
{
  while (*s != '\0' && !isblank (*s))
    ++s;
  return s;
}

#ifdef WIN32
/*
 * Same as end_of_token, but take into account a stop character
 */
char *
end_of_token_w32 (s, stopchar)
     char *s;
     char stopchar;
{
  register char *p = s;
  register int backslash = 0;

  while (*p != '\0' && *p != stopchar && (backslash || !isblank (*p)))
    {
      if (*p++ == '\\')
        {
          backslash = !backslash;
          while (*p == '\\')
            {
              backslash = !backslash;
              ++p;
            }
        }
      else
        backslash = 0;
    }

  return p;
}
#endif

/* Return the address of the first nonwhitespace or null in the string S.  */

char *
next_token (s)
     char *s;
{
  register char *p = s;

  while (isblank (*p))
    ++p;
  return p;
}

/* Find the next token in PTR; return the address of it, and store the
   length of the token into *LENGTHPTR if LENGTHPTR is not nil.  */

char *
find_next_token (ptr, lengthptr)
     char **ptr;
     unsigned int *lengthptr;
{
  char *p = next_token (*ptr);
  char *end;

  if (*p == '\0')
    return 0;

  *ptr = end = end_of_token (p);
  if (lengthptr != 0)
    *lengthptr = end - p;
  return p;
}

/* Copy a chain of `struct dep', making a new chain
   with the same contents as the old one.  */

struct dep *
copy_dep_chain (d)
     register struct dep *d;
{
  register struct dep *c;
  struct dep *firstnew = 0;
  struct dep *lastnew;

  while (d != 0)
    {
      c = (struct dep *) xmalloc (sizeof (struct dep));
      bcopy ((char *) d, (char *) c, sizeof (struct dep));
      if (c->name != 0)
	c->name = savestring (c->name, strlen (c->name));
      c->next = 0;
      if (firstnew == 0)
	firstnew = lastnew = c;
      else
	lastnew = lastnew->next = c;

      d = d->next;
    }

  return firstnew;
}

#ifdef	iAPX286
/* The losing compiler on this machine can't handle this macro.  */

char *
dep_name (dep)
     struct dep *dep;
{
  return dep->name == 0 ? dep->file->name : dep->name;
}
#endif

#ifdef	GETLOADAVG_PRIVILEGED

#ifdef POSIX

/* Hopefully if a system says it's POSIX.1 and has the setuid and setgid
   functions, they work as POSIX.1 says.  Some systems (Alpha OSF/1 1.2,
   for example) which claim to be POSIX.1 also have the BSD setreuid and
   setregid functions, but they don't work as in BSD and only the POSIX.1
   way works.  */

#undef HAVE_SETREUID
#undef HAVE_SETREGID

#else	/* Not POSIX.  */

/* Some POSIX.1 systems have the seteuid and setegid functions.  In a
   POSIX-like system, they are the best thing to use.  However, some
   non-POSIX systems have them too but they do not work in the POSIX style
   and we must use setreuid and setregid instead.  */

#undef HAVE_SETEUID
#undef HAVE_SETEGID

#endif	/* POSIX.  */

#ifndef	HAVE_UNISTD_H
extern int getuid (), getgid (), geteuid (), getegid ();
extern int setuid (), setgid ();
#ifdef HAVE_SETEUID
extern int seteuid ();
#else
#ifdef	HAVE_SETREUID
extern int setreuid ();
#endif	/* Have setreuid.  */
#endif	/* Have seteuid.  */
#ifdef HAVE_SETEGID
extern int setegid ();
#else
#ifdef	HAVE_SETREGID
extern int setregid ();
#endif	/* Have setregid.  */
#endif	/* Have setegid.  */
#endif	/* No <unistd.h>.  */

/* Keep track of the user and group IDs for user- and make- access.  */
static int user_uid = -1, user_gid = -1, make_uid = -1, make_gid = -1;
#define	access_inited	(user_uid != -1)
static enum { make, user } current_access;


/* Under -d, write a message describing the current IDs.  */

static void
log_access (flavor)
     char *flavor;
{
  if (! debug_flag)
    return;

  /* All the other debugging messages go to stdout,
     but we write this one to stderr because it might be
     run in a child fork whose stdout is piped.  */

  fprintf (stderr, "%s access: user %d (real %d), group %d (real %d)\n",
	   flavor, geteuid (), getuid (), getegid (), getgid ());
  fflush (stderr);
}


static void
init_access ()
{
#ifndef VMS
  user_uid = getuid ();
  user_gid = getgid ();

  make_uid = geteuid ();
  make_gid = getegid ();

  /* Do these ever fail?  */
  if (user_uid == -1 || user_gid == -1 || make_uid == -1 || make_gid == -1)
    pfatal_with_name ("get{e}[gu]id");

  log_access ("Initialized");

  current_access = make;
#endif
}

#endif	/* GETLOADAVG_PRIVILEGED */

/* Give the process appropriate permissions for access to
   user data (i.e., to stat files, or to spawn a child process).  */
void
user_access ()
{
#ifdef	GETLOADAVG_PRIVILEGED

  if (!access_inited)
    init_access ();

  if (current_access == user)
    return;

  /* We are in "make access" mode.  This means that the effective user and
     group IDs are those of make (if it was installed setuid or setgid).
     We now want to set the effective user and group IDs to the real IDs,
     which are the IDs of the process that exec'd make.  */

#ifdef	HAVE_SETEUID

  /* Modern systems have the seteuid/setegid calls which set only the
     effective IDs, which is ideal.  */

  if (seteuid (user_uid) < 0)
    pfatal_with_name ("user_access: seteuid");

#else	/* Not HAVE_SETEUID.  */

#ifndef	HAVE_SETREUID

  /* System V has only the setuid/setgid calls to set user/group IDs.
     There is an effective ID, which can be set by setuid/setgid.
     It can be set (unless you are root) only to either what it already is
     (returned by geteuid/getegid, now in make_uid/make_gid),
     the real ID (return by getuid/getgid, now in user_uid/user_gid),
     or the saved set ID (what the effective ID was before this set-ID
     executable (make) was exec'd).  */

  if (setuid (user_uid) < 0)
    pfatal_with_name ("user_access: setuid");

#else	/* HAVE_SETREUID.  */

  /* In 4BSD, the setreuid/setregid calls set both the real and effective IDs.
     They may be set to themselves or each other.  So you have two alternatives
     at any one time.  If you use setuid/setgid, the effective will be set to
     the real, leaving only one alternative.  Using setreuid/setregid, however,
     you can toggle between your two alternatives by swapping the values in a
     single setreuid or setregid call.  */

  if (setreuid (make_uid, user_uid) < 0)
    pfatal_with_name ("user_access: setreuid");

#endif	/* Not HAVE_SETREUID.  */
#endif	/* HAVE_SETEUID.  */

#ifdef	HAVE_SETEGID
  if (setegid (user_gid) < 0)
    pfatal_with_name ("user_access: setegid");
#else
#ifndef	HAVE_SETREGID
  if (setgid (user_gid) < 0)
    pfatal_with_name ("user_access: setgid");
#else
  if (setregid (make_gid, user_gid) < 0)
    pfatal_with_name ("user_access: setregid");
#endif
#endif

  current_access = user;

  log_access ("User");

#endif	/* GETLOADAVG_PRIVILEGED */
}

/* Give the process appropriate permissions for access to
   make data (i.e., the load average).  */
void
make_access ()
{
#ifdef	GETLOADAVG_PRIVILEGED

  if (!access_inited)
    init_access ();

  if (current_access == make)
    return;

  /* See comments in user_access, above.  */

#ifdef	HAVE_SETEUID
  if (seteuid (make_uid) < 0)
    pfatal_with_name ("make_access: seteuid");
#else
#ifndef	HAVE_SETREUID
  if (setuid (make_uid) < 0)
    pfatal_with_name ("make_access: setuid");
#else
  if (setreuid (user_uid, make_uid) < 0)
    pfatal_with_name ("make_access: setreuid");
#endif
#endif

#ifdef	HAVE_SETEGID
  if (setegid (make_gid) < 0)
    pfatal_with_name ("make_access: setegid");
#else
#ifndef	HAVE_SETREGID
  if (setgid (make_gid) < 0)
    pfatal_with_name ("make_access: setgid");
#else
  if (setregid (user_gid, make_gid) < 0)
    pfatal_with_name ("make_access: setregid");
#endif
#endif

  current_access = make;

  log_access ("Make");

#endif	/* GETLOADAVG_PRIVILEGED */
}

/* Give the process appropriate permissions for a child process.
   This is like user_access, but you can't get back to make_access.  */
void
child_access ()
{
#ifdef	GETLOADAVG_PRIVILEGED

  if (!access_inited)
    abort ();

  /* Set both the real and effective UID and GID to the user's.
     They cannot be changed back to make's.  */

#ifndef	HAVE_SETREUID
  if (setuid (user_uid) < 0)
    pfatal_with_name ("child_access: setuid");
#else
  if (setreuid (user_uid, user_uid) < 0)
    pfatal_with_name ("child_access: setreuid");
#endif

#ifndef	HAVE_SETREGID
  if (setgid (user_gid) < 0)
    pfatal_with_name ("child_access: setgid");
#else
  if (setregid (user_gid, user_gid) < 0)
    pfatal_with_name ("child_access: setregid");
#endif

  log_access ("Child");

#endif	/* GETLOADAVG_PRIVILEGED */
}

#ifdef NEED_GET_PATH_MAX
unsigned int
get_path_max ()
{
  static unsigned int value;

  if (value == 0)
    {
      long int x = pathconf ("/", _PC_PATH_MAX);
      if (x > 0)
	value = x;
      else
	return MAXPATHLEN;
    }

  return value;
}
#endif



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         main.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Argument parsing and main program of GNU Make.
Copyright (C) 1988, 89, 90, 91, 94, 95, 96 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "dep.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "variable.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "commands.h"  <- modification by J.Ruthruff, 7/20 */
#include "getopt.h"
#include <assert.h>
#undef stderr
#define stderr stdout

#ifdef _AMIGA
#   include <dos/dos.h>
#   include <proto/dos.h>
#undef stderr
#define stderr stdout
#endif
#ifdef WIN32
#include <windows.h>
#include "pathstuff.h"
#undef stderr
#define stderr stdout
#endif

#ifdef _AMIGA
int __stack = 20000; /* Make sure we have 20K of stack space */
#endif

extern void init_dir PARAMS ((void));
extern void remote_setup PARAMS ((void));
extern void remote_cleanup PARAMS ((void));
extern RETSIGTYPE fatal_error_signal PARAMS ((int sig));
extern RETSIGTYPE child_handler PARAMS ((int sig));

extern void print_variable_data_base PARAMS ((void));
extern void print_dir_data_base PARAMS ((void));
extern void print_rule_data_base PARAMS ((void));
extern void print_file_data_base PARAMS ((void));
extern void print_vpath_data_base PARAMS ((void));

#ifndef	HAVE_UNISTD_H
extern int chdir ();
#endif
#ifndef	STDC_HEADERS
#ifndef	sun			/* Sun has an incorrect decl in a header.  */
extern void exit ();
#endif
extern double atof ();
#endif
extern char *mktemp ();

static void print_data_base PARAMS((void));
static void print_version PARAMS ((void));
static void decode_switches PARAMS ((int argc, char **argv, int env));
static void decode_env_switches PARAMS ((char *envar, unsigned int len));
static void define_makeflags PARAMS ((int all, int makefile));
static char *quote_as_word PARAMS ((char *out, char *in, int double_dollars));

/* The structure that describes an accepted command switch.  */

struct command_switch
  {
    char c;			/* The switch character.  */

    enum			/* Type of the value.  */
      {
	flag,			/* Turn int flag on.  */
	flag_off,		/* Turn int flag off.  */
	string,			/* One string per switch.  */
	positive_int,		/* A positive integer.  */
	floating,		/* A floating-point number (double).  */
	ignore			/* Ignored.  */
      } type;

    char *value_ptr;	/* Pointer to the value-holding variable.  */

    unsigned int env:1;		/* Can come from MAKEFLAGS.  */
    unsigned int toenv:1;	/* Should be put in MAKEFLAGS.  */
    unsigned int no_makefile:1;	/* Don't propagate when remaking makefiles.  */

    char *noarg_value;	/* Pointer to value used if no argument is given.  */
    char *default_value;/* Pointer to default value.  */

    char *long_name;		/* Long option name.  */
    char *argdesc;		/* Descriptive word for argument.  */
    char *description;		/* Description for usage message.  */
  };


/* The structure used to hold the list of strings given
   in command switches of a type that takes string arguments.  */

struct stringlist
  {
    char **list;	/* Nil-terminated list of strings.  */
    unsigned int idx;	/* Index into above.  */
    unsigned int max;	/* Number of pointers allocated.  */
  };


/* The recognized command switches.  */

/* Nonzero means do not print commands to be executed (-s).  */

int silent_flag;

/* Nonzero means just touch the files
   that would appear to need remaking (-t)  */

int touch_flag;

/* Nonzero means just print what commands would need to be executed,
   don't actually execute them (-n).  */

int just_print_flag;

/* Print debugging trace info (-d).  */

int debug_flag = 0;

#ifdef WIN32
/* Suspend make in main for a short time to allow debugger to attach */

int suspend_flag = 0;
#endif

/* Environment variables override makefile definitions.  */

int env_overrides = 0;

/* Nonzero means ignore status codes returned by commands
   executed to remake files.  Just treat them all as successful (-i).  */

int ignore_errors_flag = 0;

/* Nonzero means don't remake anything, just print the data base
   that results from reading the makefile (-p).  */

int print_data_base_flag = 0;

/* Nonzero means don't remake anything; just return a nonzero status
   if the specified targets are not up to date (-q).  */

int question_flag = 0;

/* Nonzero means do not use any of the builtin rules (-r).  */

int no_builtin_rules_flag = 0;

/* Nonzero means keep going even if remaking some file fails (-k).  */

int keep_going_flag;
int default_keep_going_flag = 0;

/* Nonzero means print directory before starting and when done (-w).  */

int print_directory_flag = 0;

/* Nonzero means ignore print_directory_flag and never print the directory.
   This is necessary because print_directory_flag is set implicitly.  */

int inhibit_print_directory_flag = 0;

/* Nonzero means print version information.  */

int print_version_flag = 0;

/* List of makefiles given with -f switches.  */

static struct stringlist *makefiles = 0;


/* Number of job slots (commands that can be run at once).  */

unsigned int job_slots = 1;
unsigned int default_job_slots = 1;

/* Value of job_slots that means no limit.  */

static unsigned int inf_jobs = 0;

/* Maximum load average at which multiple jobs will be run.
   Negative values mean unlimited, while zero means limit to
   zero load (which could be useful to start infinite jobs remotely
   but one at a time locally).  */
#ifndef NO_FLOAT
double max_load_average = -1.0;
double default_load_average = -1.0;
#else
int max_load_average = -1;
int default_load_average = -1;
#endif

/* List of directories given with -C switches.  */

static struct stringlist *directories = 0;

/* List of include directories given with -I switches.  */

static struct stringlist *include_directories = 0;

/* List of files given with -o switches.  */

static struct stringlist *old_files = 0;

/* List of files given with -W switches.  */

static struct stringlist *new_files = 0;

/* If nonzero, we should just print usage and exit.  */

static int print_usage_flag = 0;

/* If nonzero, we should print a warning message
   for each reference to an undefined variable.  */

int warn_undefined_variables_flag;

/* The table of command switches.  */

static const struct command_switch switches[] =
  {
    { 'b', ignore, 0, 0, 0, 0, 0, 0,
	0, 0,
	"Ignored for compatibility" },
    { 'C', string, (char *) &directories, 0, 0, 0, 0, 0,
	"directory", "DIRECTORY",
	"Change to DIRECTORY before doing anything" },
    { 'd', flag, (char *) &debug_flag, 1, 1, 0, 0, 0,
	"debug", 0,
	"Print lots of debugging information" },
#ifdef WIN32
    { 'D', flag, (char *) &suspend_flag, 1, 1, 0, 0, 0,
        "suspend-for-debug", 0,
        "Suspend process to allow a debugger to attach" },
#endif
    { 'e', flag, (char *) &env_overrides, 1, 1, 0, 0, 0,
	"environment-overrides", 0,
	"Environment variables override makefiles" },
    { 'f', string, (char *) &makefiles, 0, 0, 0, 0, 0,
	"file", "FILE",
	"Read FILE as a makefile" },
    { 'h', flag, (char *) &print_usage_flag, 0, 0, 0, 0, 0,
	"help", 0,
	"Print this message and exit" },
    { 'i', flag, (char *) &ignore_errors_flag, 1, 1, 0, 0, 0,
	"ignore-errors", 0,
	"Ignore errors from commands" },
    { 'I', string, (char *) &include_directories, 1, 1, 0, 0, 0,
	"include-dir", "DIRECTORY",
	"Search DIRECTORY for included makefiles" },
    { 'j', positive_int, (char *) &job_slots, 1, 1, 0,
	(char *) &inf_jobs, (char *) &default_job_slots,
	"jobs", "N",
	"Allow N jobs at once; infinite jobs with no arg" },
    { 'k', flag, (char *) &keep_going_flag, 1, 1, 0,
	0, (char *) &default_keep_going_flag,
	"keep-going", 0,
	"Keep going when some targets can't be made" },
#ifndef NO_FLOAT
    { 'l', floating, (char *) &max_load_average, 1, 1, 0,
	(char *) &default_load_average, (char *) &default_load_average,
	"load-average", "N",
	"Don't start multiple jobs unless load is below N" },
#else
    { 'l', positive_int, (char *) &max_load_average, 1, 1, 0,
	(char *) &default_load_average, (char *) &default_load_average,
	"load-average", "N",
	"Don't start multiple jobs unless load is below N" },
#endif
    { 'm', ignore, 0, 0, 0, 0, 0, 0,
	0, 0,
	"-b" },
    { 'n', flag, (char *) &just_print_flag, 1, 1, 1, 0, 0,
	"just-print", 0,
	"Don't actually run any commands; just print them" },
    { 'o', string, (char *) &old_files, 0, 0, 0, 0, 0,
	"old-file", "FILE",
	"Consider FILE to be very old and don't remake it" },
    { 'p', flag, (char *) &print_data_base_flag, 1, 1, 0, 0, 0,
	"print-data-base", 0,
	"Print make's internal database" },
    { 'q', flag, (char *) &question_flag, 1, 1, 1, 0, 0,
	"question", 0,
	"Run no commands; exit status says if up to date" },
    { 'r', flag, (char *) &no_builtin_rules_flag, 1, 1, 0, 0, 0,
	"no-builtin-rules", 0,
	"Disable the built-in implicit rules" },
    { 's', flag, (char *) &silent_flag, 1, 1, 0, 0, 0,
	"silent", 0,
	"Don't echo commands" },
    { 'S', flag_off, (char *) &keep_going_flag, 1, 1, 0,
	0, (char *) &default_keep_going_flag,
	"no-keep-going", 0,
	"Turns off -k" },
    { 't', flag, (char *) &touch_flag, 1, 1, 1, 0, 0,
	"touch", 0,
	"Touch targets instead of remaking them" },
    { 'v', flag, (char *) &print_version_flag, 1, 1, 0, 0, 0,
	"version", 0,
	"Print the version number of make and exit" },
    { 'w', flag, (char *) &print_directory_flag, 1, 1, 0, 0, 0,
	"print-directory", 0,
	"Print the current directory" },
    { 2, flag, (char *) &inhibit_print_directory_flag, 1, 1, 0, 0, 0,
	"no-print-directory", 0,
	"Turn off -w, even if it was turned on implicitly" },
    { 'W', string, (char *) &new_files, 0, 0, 0, 0, 0,
	"what-if", "FILE",
	"Consider FILE to be infinitely new" },
    { 3, flag, (char *) &warn_undefined_variables_flag, 1, 1, 0, 0, 0,
	"warn-undefined-variables", 0,
	"Warn when an undefined variable is referenced" },
    { '\0', }
  };

/* Secondary long names for options.  */

static struct option long_option_aliases[] =
  {
    { "quiet",		no_argument,		0, 's' },
    { "stop",		no_argument,		0, 'S' },
    { "new-file",	required_argument,	0, 'W' },
    { "assume-new",	required_argument,	0, 'W' },
    { "assume-old",	required_argument,	0, 'o' },
    { "max-load",	optional_argument,	0, 'l' },
    { "dry-run",	no_argument,		0, 'n' },
    { "recon",		no_argument,		0, 'n' },
    { "makefile",	required_argument,	0, 'f' },
  };

/* The usage message prints the descriptions of options starting in
   this column.  Make sure it leaves enough room for the longest
   description to fit in less than 80 characters.  */

#define	DESCRIPTION_COLUMN	30

/* List of goal targets.  */

static struct dep *goals, *lastgoal;

/* List of variables which were defined on the command line
   (or, equivalently, in MAKEFLAGS).  */

struct command_variable
  {
    struct command_variable *next;
    struct variable *variable;
  };
static struct command_variable *command_variables;

/* The name we were invoked with.  */

char *program;

/* Our current directory before processing any -C options.  */

char *directory_before_chdir;

/* Our current directory after processing all -C options.  */

char *starting_directory;

/* Value of the MAKELEVEL variable at startup (or 0).  */

unsigned int makelevel;

/* First file defined in the makefile whose name does not
   start with `.'.  This is the default to remake if the
   command line does not specify.  */

struct file *default_goal_file;

/* Pointer to structure for the file .DEFAULT
   whose commands are used for any file that has none of its own.
   This is zero if the makefiles do not define .DEFAULT.  */

struct file *default_file;

/* Nonzero if we have seen the magic `.POSIX' target.
   This turns on pedantic compliance with POSIX.2.  */

int posix_pedantic;

/* Mask of signals that are being caught with fatal_error_signal.  */

#ifdef	POSIX
sigset_t fatal_signal_set;
#else
#ifdef	HAVE_SIGSETMASK
int fatal_signal_mask;
#endif
#endif

static struct file *
enter_command_line_file (name)
     char *name;
{
  if (name[0] == '\0')
    fatal ("empty string invalid as file name");

  if (name[0] == '~')
    {
      char *expanded = tilde_expand (name);
      if (expanded != 0)
	name = expanded;	/* Memory leak; I don't care.  */
    }

  /* This is also done in parse_file_seq, so this is redundant
     for names read from makefiles.  It is here for names passed
     on the command line.  */
  while (name[0] == '.' && name[1] == '/' && name[2] != '\0')
    {
      name += 2;
      while (*name == '/')
	/* Skip following slashes: ".//foo" is "foo", not "/foo".  */
	++name;
    }

  if (*name == '\0')
    {
      /* It was all slashes!  Move back to the dot and truncate
	 it after the first slash, so it becomes just "./".  */
      do
	--name;
      while (name[0] != '.');
      name[2] = '\0';
    }

  return enter_file (savestring (name, strlen (name)));
}

/* Toggle -d on receipt of SIGUSR1.  */

static RETSIGTYPE
debug_signal_handler (sig)
     int sig;
{
  debug_flag = ! debug_flag;
}

#ifndef _AMIGA
int
main (argc, argv, envp)
     int argc;
     char **argv;
     char **envp;
#else
int main (int argc, char ** argv)
#endif
{
  register struct file *f;
  register unsigned int i;
  char **p;
  struct dep *read_makefiles;
  PATH_VAR (current_directory);
#ifdef WIN32
  extern int no_default_sh_exe;
  char *unix_path = NULL;
  char *win32_path = NULL;
#endif

  default_goal_file = 0;
  reading_filename = 0;
  reading_lineno_ptr = 0;

#if !defined (HAVE_STRSIGNAL) && !defined (HAVE_SYS_SIGLIST)
  signame_init ();
#endif

#ifdef	POSIX
  sigemptyset (&fatal_signal_set);
#define	ADD_SIG(sig)	sigaddset (&fatal_signal_set, sig)
#else
#ifdef	HAVE_SIGSETMASK
  fatal_signal_mask = 0;
#define	ADD_SIG(sig)	fatal_signal_mask |= sigmask (sig)
#else
#define	ADD_SIG(sig)
#endif
#endif

#define	FATAL_SIG(sig)							      \
  if (signal ((sig), fatal_error_signal) == SIG_IGN)			      \
    (void) signal ((sig), SIG_IGN);					      \
  else									      \
    ADD_SIG (sig);

#ifdef SIGHUP
  FATAL_SIG (SIGHUP);
#endif
#ifdef SIGQUIT
  FATAL_SIG (SIGQUIT);
#endif
  FATAL_SIG (SIGINT);
  FATAL_SIG (SIGTERM);

#ifdef	SIGDANGER
  FATAL_SIG (SIGDANGER);
#endif
#ifdef SIGXCPU
  FATAL_SIG (SIGXCPU);
#endif
#ifdef SIGXFSZ
  FATAL_SIG (SIGXFSZ);
#endif

#undef	FATAL_SIG

  /* Make sure stdout is line-buffered.  */

#ifdef	HAVE_SETLINEBUF
  setlinebuf (stdout);
#else
#ifndef	SETVBUF_REVERSED
  setvbuf (stdout, (char *) 0, _IOLBF, BUFSIZ);
#else	/* setvbuf not reversed.  */
  /* Some buggy systems lose if we pass 0 instead of allocating ourselves.  */
  setvbuf (stdout, _IOLBF, xmalloc (BUFSIZ), BUFSIZ);
#endif	/* setvbuf reversed.  */
#endif	/* setlinebuf missing.  */

  /* Figure out where this program lives.  */

  if (argv[0] == 0)
    argv[0] = "";
  if (argv[0][0] == '\0')
    program = "make";
  else
    {
#ifdef VMS
      program = rindex (argv[0], ']');
#else
      program = rindex (argv[0], '/');
#endif
#ifdef __MSDOS__
      if (program == 0)
	program = rindex (argv[0], '\\');
      if (program == 0)
	program = rindex (argv[0], ':');
#endif
      if (program == 0)
	program = argv[0];
      else
	++program;
    }

  /* Set up to access user data (files).  */
  user_access ();

  /* Figure out where we are.  */

#ifdef WIN32
  if (getcwd_fs (current_directory, GET_PATH_MAX) == 0)
#else
  if (getcwd (current_directory, GET_PATH_MAX) == 0)
#endif
    {
#ifdef	HAVE_GETCWD
      perror_with_name ("getcwd: ", "");
#else
      error ("getwd: %s", current_directory);
#endif
      current_directory[0] = '\0';
      directory_before_chdir = 0;
    }
  else
    directory_before_chdir = savestring (current_directory,
					 strlen (current_directory));

  /* Read in variables from the environment.  It is important that this be
     done before $(MAKE) is are figured out so its definitions will not be
     one from the environment.  */

#ifndef _AMIGA
  for (i = 0; envp[i] != 0; ++i)
    {
      register char *ep = envp[i];
      while (*ep != '=')
	++ep;
#ifdef WIN32
      if (!strncmp(ep, "PATH", 4))
        unix_path = &ep[5];
      if (!strncmp(ep, "Path", 4))
        win32_path = &ep[5];
#endif
      /* The result of pointer arithmetic is cast to unsigned int for
	 machines where ptrdiff_t is a different size that doesn't widen
	 the same.  */
      define_variable (envp[i], (unsigned int) (ep - envp[i]),
		       ep + 1, o_env, 1)
	/* Force exportation of every variable culled from the environment.
	   We used to rely on target_environment's v_default code to do this.
	   But that does not work for the case where an environment variable
	   is redefined in a makefile with `override'; it should then still
	   be exported, because it was originally in the environment.  */
	->export = v_export;
    }
#ifdef WIN32
    /*
     * PATH defaults to Path iff PATH not found and Path is found.
     */
    if (!unix_path && win32_path)
      define_variable("PATH", 4, win32_path, o_env, 1)->export = v_export;
#endif
#else /* For Amiga, read the ENV: device, ignoring all dirs */
    {
	BPTR env, file, old;
	char buffer[1024];
	int len;
	__aligned struct FileInfoBlock fib;

	env = Lock ("ENV:", ACCESS_READ);
	if (env)
	{
	    old = CurrentDir (DupLock(env));
	    Examine (env, &fib);

	    while (ExNext (env, &fib))
	    {
		if (fib.fib_DirEntryType < 0) /* File */
		{
		    /* Define an empty variable. It will be filled in
			variable_lookup(). Makes startup quite a bit
			faster. */
			define_variable (fib.fib_FileName,
			    strlen (fib.fib_FileName),
			"", o_env, 1)->export = v_export;
		}
	    }
	    UnLock (env);
	    UnLock(CurrentDir(old));
	}
    }
#endif

  /* Decode the switches.  */

  decode_env_switches ("MAKEFLAGS", 9);
#if 0
  /* People write things like:
     	MFLAGS="CC=gcc -pipe" "CFLAGS=-g"
     and we set the -p, -i and -e switches.  Doesn't seem quite right.  */
  decode_env_switches ("MFLAGS", 6);
#endif
  decode_switches (argc, argv, 0);
#ifdef WIN32
  if (suspend_flag) {
        fprintf(stderr, "%s (pid = %d)\n", argv[0], GetCurrentProcessId());
        fprintf(stderr, "%s is suspending for 30 seconds...", argv[0]);
        Sleep(30 * 1000);
        fprintf(stderr, "done sleep(30). Continuing.\n");
  }
#endif

  /* Print version information.  */

  if (print_version_flag || print_data_base_flag || debug_flag)
    print_version ();

  /* `make --version' is supposed to just print the version and exit.  */
  if (print_version_flag)
    die (0);

#if !defined(__MSDOS__) && !defined(VMS)
  /* Set the "MAKE_COMMAND" variable to the name we were invoked with.
     (If it is a relative pathname with a slash, prepend our directory name
     so the result will run the same program regardless of the current dir.
     If it is a name with no slash, we can only hope that PATH did not
     find it in the current directory.)  */
#ifdef WIN32
  /*
   * Convert from backslashes to forward slashes for
   * programs like sh which don't like them. Shouldn't
   * matter if the path is one way or the other for
   * CreateProcess().
   */
  if (strpbrk(argv[0], "/:\\") ||
      strstr(argv[0], "..") ||
      !strncmp(argv[0], "//", 2))
    argv[0] = strdup(w32ify(argv[0],1));
#else /* WIN32 */
  if (current_directory[0] != '\0'
      && argv[0] != 0 && argv[0][0] != '/' && index (argv[0], '/') != 0)
    argv[0] = concat (current_directory, "/", argv[0]);
#endif /* WIN32 */
#endif

  /* The extra indirection through $(MAKE_COMMAND) is done
     for hysterical raisins.  */
  (void) define_variable ("MAKE_COMMAND", 12, argv[0], o_default, 0);
  (void) define_variable ("MAKE", 4, "$(MAKE_COMMAND)", o_default, 1);

  if (command_variables != 0)
    {
      struct command_variable *cv;
      struct variable *v;
      unsigned int len = 0;
      char *value, *p;

      /* Figure out how much space will be taken up by the command-line
	 variable definitions.  */
      for (cv = command_variables; cv != 0; cv = cv->next)
	{
	  v = cv->variable;
	  len += 2 * strlen (v->name);
	  if (! v->recursive)
	    ++len;
	  ++len;
	  len += 2 * strlen (v->value);
	}

      /* Now allocate a buffer big enough and fill it.  */
      p = value = (char *) alloca (len);
      for (cv = command_variables; cv != 0; cv = cv->next)
	{
	  v = cv->variable;
	  p = quote_as_word (p, v->name, 0);
	  if (! v->recursive)
	    *p++ = ':';
	  *p++ = '=';
	  p = quote_as_word (p, v->value, 0);
	  *p++ = ' ';
	}
      p[-1] = '\0';		/* Kill the final space and terminate.  */

      /* Define an unchangeable variable with a name that no POSIX.2
	 makefile could validly use for its own variable.  */
      (void) define_variable ("-*-command-variables-*-", 23,
			      value, o_automatic, 0);

      /* Define the variable; this will not override any user definition.
         Normally a reference to this variable is written into the value of
         MAKEFLAGS, allowing the user to override this value to affect the
         exported value of MAKEFLAGS.  In POSIX-pedantic mode, we cannot
         allow the user's setting of MAKEOVERRIDES to affect MAKEFLAGS, so
         a reference to this hidden variable is written instead. */
      (void) define_variable ("MAKEOVERRIDES", 13,
			      "${-*-command-variables-*-}", o_env, 1);
    }

  /* If there were -C flags, move ourselves about.  */
  if (directories != 0)
    for (i = 0; directories->list[i] != 0; ++i)
      {
	char *dir = directories->list[i];
	if (dir[0] == '~')
	  {
	    char *expanded = tilde_expand (dir);
	    if (expanded != 0)
	      dir = expanded;
	  }
	if (chdir (dir) < 0)
	  pfatal_with_name (dir);
	if (dir != directories->list[i])
	  free (dir);
      }

#ifdef WIN32
  /*
   * THIS BLOCK OF CODE MUST COME AFTER chdir() CALL ABOVE IN ORDER
   * TO NOT CONFUSE THE DEPENDENCY CHECKING CODE IN implicit.c.
   *
   * The functions in dir.c can incorrectly cache information for "."
   * before we have changed directory and this can cause file
   * lookups to fail because the current directory (.) was pointing
   * at the wrong place when it was first evaluated.
   */

  /*
   * On Windows/NT, we don't have the luxury of a /bin directory that
   * is mapped globally to every drive mounted to the system. Since make could
   * be invoked from any drive, and we don't want to propogate /bin/sh
   * to every single drive. Allow ourselves a chance to search for
   * a value for default shell here (if the default path does not exist).
   *
   * The value of default_shell is set here, but it could get reset after
   * the Makefiles are read in. See logic below where SHELL is checked
   * after the call to read_all_makefiles() completes.
   *
   * The reason SHELL is set here is so that macros can be safely evaluated
   * as makefiles are read in (some macros require $SHELL).
   */

  {
    extern char *default_shell;

    if (!file_exists_p(default_shell)) {
      char *p;
      struct variable *v = lookup_variable ("Path", 4);

      /*
       * Try and make sure we have a full path to default_shell before
       * we parse makefiles.
       */
      if (v && v->value) {
        PATH_VAR(sh_path);
        char *ep;

        p  = v->value;
        ep = strchr(p, PATH_SEPARATOR_CHAR);

        while (ep && *ep) {
          *ep = '\0';

          if (dir_file_exists_p(p, default_shell)) {
            sprintf(sh_path, "%s/%s", p, default_shell);
            default_shell = strdup(w32ify(sh_path,0));
            no_default_sh_exe = 0;
            *ep = PATH_SEPARATOR_CHAR;

            /* terminate loop */
            p += strlen(p);
          } else {
            *ep = PATH_SEPARATOR_CHAR;
             p = ++ep;
          }

          ep = strchr(p, PATH_SEPARATOR_CHAR);
        }

        /* be sure to check last element of Path */
        if (p && *p && dir_file_exists_p(p, default_shell)) {
            sprintf(sh_path, "%s/%s", p, default_shell);
            default_shell = strdup(w32ify(sh_path,0));
            no_default_sh_exe = 0;
        }
      }
    }
  }
#endif /* WIN32 */
  /* Figure out the level of recursion.  */
  {
    struct variable *v = lookup_variable ("MAKELEVEL", 9);
    if (v != 0 && *v->value != '\0' && *v->value != '-')
      makelevel = (unsigned int) atoi (v->value);
    else
      makelevel = 0;
  }

  /* Except under -s, always do -w in sub-makes and under -C.  */
  if (!silent_flag && (directories != 0 || makelevel > 0))
    print_directory_flag = 1;

  /* Let the user disable that with --no-print-directory.  */
  if (inhibit_print_directory_flag)
    print_directory_flag = 0;

  /* Construct the list of include directories to search.  */

  construct_include_path (include_directories == 0 ? (char **) 0
			  : include_directories->list);

  /* Figure out where we are now, after chdir'ing.  */
  if (directories == 0)
    /* We didn't move, so we're still in the same place.  */
    starting_directory = current_directory;
  else
    {
#ifdef WIN32
      if (getcwd_fs (current_directory, GET_PATH_MAX) == 0)
#else
      if (getcwd (current_directory, GET_PATH_MAX) == 0)
#endif
	{
#ifdef	HAVE_GETCWD
	  perror_with_name ("getcwd: ", "");
#else
	  error ("getwd: %s", current_directory);
#endif
	  starting_directory = 0;
	}
      else
	starting_directory = current_directory;
    }

  /* Read any stdin makefiles into temporary files.  */

  if (makefiles != 0)
    {
      register unsigned int i;
      for (i = 0; i < makefiles->idx; ++i)
	if (makefiles->list[i][0] == '-' && makefiles->list[i][1] == '\0')
	  {
	    /* This makefile is standard input.  Since we may re-exec
	       and thus re-read the makefiles, we read standard input
	       into a temporary file and read from that.  */
	    FILE *outfile;

	    /* Make a unique filename.  */
#ifdef HAVE_MKTEMP

#ifdef VMS
	    static char name[] = "sys$scratch:GmXXXXXX";
#else
	    static char name[] = "/tmp/GmXXXXXX";
#endif
	    (void) mktemp (name);
#else
	    static char name[L_tmpnam];
	    (void) tmpnam (name);
#endif

	    outfile = fopen (name, "w");
	    if (outfile == 0)
	      pfatal_with_name ("fopen (temporary file)");
	    while (!feof (stdin))
	      {
		char buf[2048];
		unsigned int n = fread (buf, 1, sizeof(buf), stdin);
		if (n > 0 && fwrite (buf, 1, n, outfile) != n)
		  pfatal_with_name ("fwrite (temporary file)");
	      }
	    /* Try to make sure we won't remake the temporary
	       file when we are re-exec'd.  Kludge-o-matic!  */
	    fprintf (outfile, "%s:;\n", name);
	    (void) fclose (outfile);

	    /* Replace the name that read_all_makefiles will
	       see with the name of the temporary file.  */
	    {
	      char *temp;
	      /* SGI compiler requires alloca's result be assigned simply.  */
	      temp = (char *) alloca (sizeof (name));
	      bcopy (name, temp, sizeof (name));
	      makefiles->list[i] = temp;
	    }

	    /* Make sure the temporary file will not be remade.  */
	    f = enter_file (savestring (name, sizeof name - 1));
	    f->updated = 1;
	    f->update_status = 0;
	    f->command_state = cs_finished;
	    /* Let it be removed when we're done.  */
	    f->intermediate = 1;
	    /* But don't mention it.  */
	    f->dontcare = 1;
	  }
    }

  /* Set up to handle children dying.  This must be done before
     reading in the makefiles so that `shell' function calls will work.  */

#ifdef SIGCHLD
  (void) signal (SIGCHLD, child_handler);
#endif
#ifdef SIGCLD
  (void) signal (SIGCLD, child_handler);
#endif

  /* Let the user send us SIGUSR1 to toggle the -d flag during the run.  */
#ifdef SIGUSR1
  (void) signal (SIGUSR1, debug_signal_handler);
#endif

  /* Define the initial list of suffixes for old-style rules.  */

  set_default_suffixes ();

  /* Define the file rules for the built-in suffix rules.  These will later
     be converted into pattern rules.  We used to do this in
     install_default_implicit_rules, but since that happens after reading
     makefiles, it results in the built-in pattern rules taking precedence
     over makefile-specified suffix rules, which is wrong.  */

  install_default_suffix_rules ();

  /* Define some internal and special variables.  */

  define_automatic_variables ();

  /* Set up the MAKEFLAGS and MFLAGS variables
     so makefiles can look at them.  */

  define_makeflags (0, 0);

#ifdef WIN32
  /*
   * Now that makefiles are parsed, see if a Makefile gave a
   * value for SHELL and use that for default_shell instead if
   * that filename exists. This should speed up the
   * construct_argv_internal() function by avoiding unnecessary
   * recursion.
   */
  {
    struct variable *v = lookup_variable("SHELL", 5);
    extern char* default_shell;

    /*
     * to change value:
     *
     * SHELL must be found, SHELL must be set, value of SHELL
     * must be different from current value, and the
     * specified file must exist. Whew!
     */
    if (v != 0 && *v->value != '\0') {
      char *fn = recursively_expand(v);

      if (fn && strcmp(fn, default_shell) && file_exists_p(fn)) {
        char *p;

        default_shell = fn;

        /* if Makefile says SHELL is sh.exe, believe it */
        if (strstr(default_shell, "sh.exe"))
               no_default_sh_exe = 0;

        /*
         * Convert from backslashes to forward slashes so
         * create_command_line_argv_internal() is not confused.
         */
        for (p = strchr(default_shell, '\\'); p; p = strchr(default_shell, '\\'))
          *p = '/';
      }
    }
  }
  if (no_default_sh_exe && job_slots != 1) {
    error("Do not specify -j or --jobs if sh.exe is not available.");
    error("Resetting make for single job mode.");
    job_slots = 1;
  }
#endif /* WIN32 */

  /* Define the default variables.  */
  define_default_variables ();

  /* Read all the makefiles.  */

  default_file = enter_file (".DEFAULT");

  read_makefiles
    = read_all_makefiles (makefiles == 0 ? (char **) 0 : makefiles->list);

  /* Decode switches again, in case the variables were set by the makefile.  */
  decode_env_switches ("MAKEFLAGS", 9);
#if 0
  decode_env_switches ("MFLAGS", 6);
#endif

  /* Set up MAKEFLAGS and MFLAGS again, so they will be right.  */

  define_makeflags (1, 0);

  /* Make each `struct dep' point at the `struct file' for the file
     depended on.  Also do magic for special targets.  */

  snap_deps ();

  /* Convert old-style suffix rules to pattern rules.  It is important to
     do this before installing the built-in pattern rules below, so that
     makefile-specified suffix rules take precedence over built-in pattern
     rules.  */

  convert_to_pattern ();

  /* Install the default implicit pattern rules.
     This used to be done before reading the makefiles.
     But in that case, built-in pattern rules were in the chain
     before user-defined ones, so they matched first.  */

  install_default_implicit_rules ();

  /* Compute implicit rule limits.  */

  count_implicit_rule_limits ();

  /* Construct the listings of directories in VPATH lists.  */

  build_vpath_lists ();

  /* Mark files given with -o flags as very old (00:00:01.00 Jan 1, 1970)
     and as having been updated already, and files given with -W flags as
     brand new (time-stamp as far as possible into the future).  */

  if (old_files != 0)
    for (p = old_files->list; *p != 0; ++p)
      {
	f = enter_command_line_file (*p);
	f->last_mtime = (time_t) 1;
	f->updated = 1;
	f->update_status = 0;
	f->command_state = cs_finished;
      }

  if (new_files != 0)
    {
      for (p = new_files->list; *p != 0; ++p)
	{
	  f = enter_command_line_file (*p);
	  f->last_mtime = NEW_MTIME;
	}
    }

  /* Initialize the remote job module.  */
  remote_setup ();

  if (read_makefiles != 0)
    {
      /* Update any makefiles if necessary.  */

      time_t *makefile_mtimes = 0;
      unsigned int mm_idx = 0;

      if (debug_flag)
	puts ("Updating makefiles....");

      /* Remove any makefiles we don't want to try to update.
	 Also record the current modtimes so we can compare them later.  */
      {
	register struct dep *d, *last;
	last = 0;
	d = read_makefiles;
	while (d != 0)
	  {
	    register struct file *f = d->file;
	    if (f->double_colon)
	      for (f = f->double_colon; f != NULL; f = f->prev)
		{
		  if (f->deps == 0 && f->cmds != 0)
		    {
		      /* This makefile is a :: target with commands, but
			 no dependencies.  So, it will always be remade.
			 This might well cause an infinite loop, so don't
			 try to remake it.  (This will only happen if
			 your makefiles are written exceptionally
			 stupidly; but if you work for Athena, that's how
			 you write your makefiles.)  */

		      if (debug_flag)
			printf ("Makefile `%s' might loop; not remaking it.\n",
				f->name);

		      if (last == 0)
			read_makefiles = d->next;
		      else
			last->next = d->next;

		      /* Free the storage.  */
		      free ((char *) d);

		      d = last == 0 ? 0 : last->next;

		      break;
		    }
		}
	    if (f == NULL || !f->double_colon)
	      {
		if (makefile_mtimes == 0)
		  makefile_mtimes = (time_t *) xmalloc (sizeof (time_t));
		else
		  makefile_mtimes = (time_t *)
		    xrealloc ((char *) makefile_mtimes,
			      (mm_idx + 1) * sizeof (time_t));
		makefile_mtimes[mm_idx++] = file_mtime_no_search (d->file);
		last = d;
		d = d->next;
	      }
	  }
      }

      /* Set up `MAKEFLAGS' specially while remaking makefiles.  */
      define_makeflags (1, 1);
      
      switch (update_goal_chain (read_makefiles, 1))
	{
	case 1: case_coverage = case_coverage; printf("**********QUESTION SWITCH STATEMENT REACHED; CASE 1:**********\n");
    default:  printf("**********QUESTION SWITCH STATEMENT REACHED; DEFAULT:**********\n");
#define BOGUS_UPDATE_STATUS 0
	  assert (BOGUS_UPDATE_STATUS);
	  break;

    case -1:  printf("**********QUESTION SWITCH STATEMENT REACHED; CASE -1:**********\n");
	  /* Did nothing.  */
	  break;

    case 2:  printf("**********QUESTION SWITCH STATEMENT REACHED; CASE 2:**********\n");
	  /* Failed to update.  Figure out if we care.  */
	  {
	    /* Nonzero if any makefile was successfully remade.  */
	    int any_remade = 0;
	    /* Nonzero if any makefile we care about failed
	       in updating or could not be found at all.  */
	    int any_failed = 0;
	    register unsigned int i;

	    for (i = 0; read_makefiles != 0; ++i)
	      {
		struct dep *d = read_makefiles;
		read_makefiles = d->next;
		if (d->file->updated)
		  {
		    /* This makefile was updated.  */
		    if (d->file->update_status == 0)
		      {
			/* It was successfully updated.  */
			any_remade |= (file_mtime_no_search (d->file)
				       != makefile_mtimes[i]);
		      }
		    else if (! (d->changed & RM_DONTCARE))
		      {
			time_t mtime;
			/* The update failed and this makefile was not
			   from the MAKEFILES variable, so we care.  */
			error ("Failed to remake makefile `%s'.",
			       d->file->name);
			mtime = file_mtime_no_search (d->file);
			any_remade |= (mtime != (time_t) -1
				       && mtime != makefile_mtimes[i]);
		      }
		  }
		else
		  /* This makefile was not found at all.  */
		  if (! (d->changed & RM_DONTCARE))
		    {
		      /* This is a makefile we care about.  See how much.  */
		      if (d->changed & RM_INCLUDED)
			/* An included makefile.  We don't need
			   to die, but we do want to complain.  */
			error ("Included makefile `%s' was not found.",
			       dep_name (d));
		      else
			{
			  /* A normal makefile.  We must die later.  */
			  error ("Makefile `%s' was not found", dep_name (d));
			  any_failed = 1;
			}
		    }

		free ((char *) d);
	      }

	    if (any_remade)
	      goto re_exec;
	    else if (any_failed)
	      die (2);
	    else
	      break;
	  }

    case 0:  printf("**********QUESTION SWITCH STATEMENT REACHED; CASE 0:**********\n");
	re_exec:
	  /* Updated successfully.  Re-exec ourselves.  */

	  remove_intermediates (0);

	  if (print_data_base_flag)
	    print_data_base ();

	  log_working_directory (0);

	  if (makefiles != 0)
	    {
	      /* These names might have changed.  */
	      register unsigned int i, j = 0;
	      for (i = 1; i < argc; ++i)
		if (!strcmp (argv[i], "-f")) /* XXX */
		  {
		    char *p = &argv[i][2];
		    if (*p == '\0')
		      argv[++i] = makefiles->list[j];
		    else
		      argv[i] = concat ("-f", makefiles->list[j], "");
		    ++j;
		  }
	    }

	  if (directories != 0 && directories->idx > 0)
	    {
	      char bad;
	      if (directory_before_chdir != 0)
		{
		  if (chdir (directory_before_chdir) < 0)
		    {
		      perror_with_name ("chdir", "");
		      bad = 1;
		    }
		  else
		    bad = 0;
		}
	      else
		bad = 1;
	      if (bad)
		fatal ("Couldn't change back to original directory.");
	    }

#ifndef _AMIGA
	  for (p = environ; *p != 0; ++p)
	    if (!strncmp (*p, "MAKELEVEL=", 10))
	      {
		/* The SGI compiler apparently can't understand
		   the concept of storing the result of a function
		   in something other than a local variable.  */
		char *sgi_loses;
		sgi_loses = (char *) alloca (40);
		*p = sgi_loses;
		sprintf (*p, "MAKELEVEL=%u", makelevel);
		break;
	      }
#else /* AMIGA */
	  {
	    char buffer[256];
	    int len;

	    len = GetVar ("MAKELEVEL", buffer, sizeof (buffer), GVF_GLOBAL_ONLY);

	    if (len != -1)
	    {
	    sprintf (buffer, "%u", makelevel);
	      SetVar ("MAKELEVEL", buffer, -1, GVF_GLOBAL_ONLY);
	    }
	  }
#endif

	  if (debug_flag)
	    {
	      char **p;
	      fputs ("Re-executing:", stdout);
	      for (p = argv; *p != 0; ++p)
		printf (" %s", *p);
	      puts ("");
	    }

	  fflush (stdout);
	  fflush (stderr);

#ifndef _AMIGA
	  exec_command (argv, environ);
#else
	  exec_command (argv);
	  exit (0);
#endif
	  /* NOTREACHED */
	}
    }

  /* Set up `MAKEFLAGS' again for the normal targets.  */
  define_makeflags (1, 0);

  {
    int status;

    /* If there were no command-line goals, use the default.  */
    if (goals == 0)
      {
	if (default_goal_file != 0)
	  {
	    goals = (struct dep *) xmalloc (sizeof (struct dep));
	    goals->next = 0;
	    goals->name = 0;
	    goals->file = default_goal_file;
	  }
      }
    else
      lastgoal->next = 0;

    if (goals != 0)
      {
	/* Update the goals.  */

	if (debug_flag)
	  puts ("Updating goal targets....");

	switch (update_goal_chain (goals, 0))
	  {
      case -1: case_coverage = case_coverage;  printf("**********QUESTION SWITCH STATEMENT #2 REACHED; CASE -1:**********\n");
	    /* Nothing happened.  */
      case 0:  printf("**********QUESTION SWITCH STATEMENT #2 REACHED; CASE 0:**********\n");
	    /* Updated successfully.  */
	    status = EXIT_SUCCESS;
	    break;
      case 2:  printf("**********QUESTION SWITCH STATEMENT #2 REACHED; CASE 2:**********\n");
	    /* Updating failed.  POSIX.2 specifies exit status >1 for this;
	       but in VMS, there is only success and failure.  */
	    status = EXIT_FAILURE ? 2 : EXIT_FAILURE;
	    break;
      case 1:  printf("**********QUESTION SWITCH STATEMENT #2 REACHED; CASE 1:**********\n");
	    /* We are under -q and would run some commands.  */
	    status = EXIT_FAILURE;
	    break;
      default:  printf("**********QUESTION SWITCH STATEMENT #2 REACHED; DEFAULT:**********\n");
	    abort ();
	  }
      }
    else
      {
	if (read_makefiles == 0)
	  fatal ("No targets specified and no makefile found");
	else
	  fatal ("No targets");
      }

    /* Exit.  */
    die (status);
  }

  return 0;
}

/* Parsing of arguments, decoding of switches.  */

static char options[1 + sizeof (switches) / sizeof (switches[0]) * 3];
static struct option long_options[(sizeof (switches) / sizeof (switches[0])) +
				  (sizeof (long_option_aliases) /
				   sizeof (long_option_aliases[0]))];

/* Fill in the string and vector for getopt.  */
static void
init_switches ()
{
  register char *p;
  register int c;
  register unsigned int i;

  if (options[0] != '\0')
    /* Already done.  */
    return;

  p = options;

  /* Return switch and non-switch args in order, regardless of
     POSIXLY_CORRECT.  Non-switch args are returned as option 1.  */
  *p++ = '-';

  for (i = 0; switches[i].c != '\0'; ++i)
    {
      long_options[i].name = (switches[i].long_name == 0 ? "" :
			      switches[i].long_name);
      long_options[i].flag = 0;
      long_options[i].val = switches[i].c;
      if (isalnum (switches[i].c))
	*p++ = switches[i].c;
      switch (switches[i].type)
	{
	case flag: case_coverage = case_coverage;
	case flag_off: case_coverage = case_coverage;
	case ignore:
	  long_options[i].has_arg = no_argument;
	  break;

	case string: case_coverage = case_coverage;
	case positive_int: case_coverage = case_coverage;
	case floating:
	  if (isalnum (switches[i].c))
	    *p++ = ':';
	  if (switches[i].noarg_value != 0)
	    {
	      if (isalnum (switches[i].c))
		*p++ = ':';
	      long_options[i].has_arg = optional_argument;
	    }
	  else
	    long_options[i].has_arg = required_argument;
	  break;
	}
    }
  *p = '\0';
  for (c = 0; c < (sizeof (long_option_aliases) /
		   sizeof (long_option_aliases[0]));
       ++c)
    long_options[i++] = long_option_aliases[c];
  long_options[i].name = 0;
}

static void
handle_non_switch_argument (arg, env)
     char *arg;
     int env;
{
  /* Non-option argument.  It might be a variable definition.  */
  struct variable *v;
  if (arg[0] == '-' && arg[1] == '\0')
    /* Ignore plain `-' for compatibility.  */
    return;
  v = try_variable_definition ((char *) 0, 0, arg, o_command);
  if (v != 0)
    {
      /* It is indeed a variable definition.  Record a pointer to
	 the variable for later use in define_makeflags.  */
      struct command_variable *cv
	= (struct command_variable *) xmalloc (sizeof (*cv));
      cv->variable = v;
      cv->next = command_variables;
      command_variables = cv;
    }
  else if (! env)
    {
      /* Not an option or variable definition; it must be a goal
	 target!  Enter it as a file and add it to the dep chain of
	 goals.  */
      struct file *f = enter_command_line_file (arg);
      f->cmd_target = 1;

      if (goals == 0)
	{
	  goals = (struct dep *) xmalloc (sizeof (struct dep));
	  lastgoal = goals;
	}
      else
	{
	  lastgoal->next
	    = (struct dep *) xmalloc (sizeof (struct dep));
	  lastgoal = lastgoal->next;
	}
      lastgoal->name = 0;
      lastgoal->file = f;
    }
}

/* Decode switches from ARGC and ARGV.
   They came from the environment if ENV is nonzero.  */

static void
decode_switches (argc, argv, env)
     int argc;
     char **argv;
     int env;
{
  int bad = 0;
  register const struct command_switch *cs;
  register struct stringlist *sl;
  register int c;

  /* getopt does most of the parsing for us.
     First, get its vectors set up.  */

  init_switches ();

  /* Let getopt produce error messages for the command line,
     but not for options from the environment.  */
  opterr = !env;
  /* Reset getopt's state.  */
  optind = 0;

  while (optind < argc)
    {
      /* Parse the next argument.  */
      c = getopt_long (argc, argv, options, long_options, (int *) 0);
      if (c == EOF)
	/* End of arguments, or "--" marker seen.  */
	break;
      else if (c == 1)
	/* An argument not starting with a dash.  */
	handle_non_switch_argument (optarg, env);
      else if (c == '?')
	/* Bad option.  We will print a usage message and die later.
	   But continue to parse the other options so the user can
	   see all he did wrong.  */
	bad = 1;
      else
	for (cs = switches; cs->c != '\0'; ++cs)
	  if (cs->c == c)
	    {
	      /* Whether or not we will actually do anything with
		 this switch.  We test this individually inside the
		 switch below rather than just once outside it, so that
		 options which are to be ignored still consume args.  */
	      int doit = !env || cs->env;

	      switch (cs->type)
		{
		default:
		  abort ();

		case ignore:
		  break;

		case flag: case_coverage = case_coverage;
		case flag_off:
		  if (doit)
		    *(int *) cs->value_ptr = cs->type == flag;
		  break;

		case string:
		  if (!doit)
		    break;

		  if (optarg == 0)
		    optarg = cs->noarg_value;

		  sl = *(struct stringlist **) cs->value_ptr;
		  if (sl == 0)
		    {
		      sl = (struct stringlist *)
			xmalloc (sizeof (struct stringlist));
		      sl->max = 5;
		      sl->idx = 0;
		      sl->list = (char **) xmalloc (5 * sizeof (char *));
		      *(struct stringlist **) cs->value_ptr = sl;
		    }
		  else if (sl->idx == sl->max - 1)
		    {
		      sl->max += 5;
		      sl->list = (char **)
			xrealloc ((char *) sl->list,
				  sl->max * sizeof (char *));
		    }
		  sl->list[sl->idx++] = optarg;
		  sl->list[sl->idx] = 0;
		  break;

		case positive_int:
		  if (optarg == 0 && argc > optind
		      && isdigit (argv[optind][0]))
		    optarg = argv[optind++];

		  if (!doit)
		    break;

		  if (optarg != 0)
		    {
		      int i = atoi (optarg);
		      if (i < 1)
			{
			  if (doit)
			    error ("the `-%c' option requires a \
positive integral argument",
				   cs->c);
			  bad = 1;
			}
		      else
			*(unsigned int *) cs->value_ptr = i;
		    }
		  else
		    *(unsigned int *) cs->value_ptr
		      = *(unsigned int *) cs->noarg_value;
		  break;

#ifndef NO_FLOAT
		case floating:
		  if (optarg == 0 && optind < argc
		      && (isdigit (argv[optind][0]) || argv[optind][0] == '.'))
		    optarg = argv[optind++];

		  if (doit)
		    *(double *) cs->value_ptr
		      = (optarg != 0 ? atof (optarg)
			 : *(double *) cs->noarg_value);

		  break;
#endif
		}

	      /* We've found the switch.  Stop looking.  */
	      break;
	    }
    }

  /* There are no more options according to getting getopt, but there may
     be some arguments left.  Since we have asked for non-option arguments
     to be returned in order, this only happens when there is a "--"
     argument to prevent later arguments from being options.  */
  while (optind < argc)
    handle_non_switch_argument (argv[optind++], env);


  if (!env && (bad || print_usage_flag))
    {
      /* Print a nice usage message.  */
      FILE *usageto;

      if (print_version_flag)
	print_version ();

      usageto = bad ? stderr : stdout;

      fprintf (usageto, "Usage: %s [options] [target] ...\n", program);

      fputs ("Options:\n", usageto);
      for (cs = switches; cs->c != '\0'; ++cs)
	{
	  char buf[1024], shortarg[50], longarg[50], *p;

	  if (cs->description[0] == '-')
	    continue;

	  switch (long_options[cs - switches].has_arg)
	    {
	    case no_argument:
	      shortarg[0] = longarg[0] = '\0';
	      break;
	    case required_argument:
	      sprintf (longarg, "=%s", cs->argdesc);
	      sprintf (shortarg, " %s", cs->argdesc);
	      break;
	    case optional_argument:
	      sprintf (longarg, "[=%s]", cs->argdesc);
	      sprintf (shortarg, " [%s]", cs->argdesc);
	      break;
	    }

	  p = buf;

	  if (isalnum (cs->c))
	    {
	      sprintf (buf, "  -%c%s", cs->c, shortarg);
	      p += strlen (p);
	    }
	  if (cs->long_name != 0)
	    {
	      unsigned int i;
	      sprintf (p, "%s--%s%s",
		       !isalnum (cs->c) ? "  " : ", ",
		       cs->long_name, longarg);
	      p += strlen (p);
	      for (i = 0; i < (sizeof (long_option_aliases) /
			       sizeof (long_option_aliases[0]));
		   ++i)
		if (long_option_aliases[i].val == cs->c)
		  {
		    sprintf (p, ", --%s%s",
			     long_option_aliases[i].name, longarg);
		    p += strlen (p);
		  }
	    }
	  {
	    const struct command_switch *ncs = cs;
	    while ((++ncs)->c != '\0')
	      if (ncs->description[0] == '-' &&
		  ncs->description[1] == cs->c)
		{
		  /* This is another switch that does the same
		     one as the one we are processing.  We want
		     to list them all together on one line.  */
		  sprintf (p, ", -%c%s", ncs->c, shortarg);
		  p += strlen (p);
		  if (ncs->long_name != 0)
		    {
		      sprintf (p, ", --%s%s", ncs->long_name, longarg);
		      p += strlen (p);
		    }
		}
	  }

	  if (p - buf > DESCRIPTION_COLUMN - 2)
	    /* The list of option names is too long to fit on the same
	       line with the description, leaving at least two spaces.
	       Print it on its own line instead.  */
	    {
	      fprintf (usageto, "%s\n", buf);
	      buf[0] = '\0';
	    }

	  fprintf (usageto, "%*s%s.\n",
		   - DESCRIPTION_COLUMN,
		   buf, cs->description);
	}

      die (bad ? 2 : 0);
    }
}

/* Decode switches from environment variable ENVAR (which is LEN chars long).
   We do this by chopping the value into a vector of words, prepending a
   dash to the first word if it lacks one, and passing the vector to
   decode_switches.  */

static void
decode_env_switches (envar, len)
     char *envar;
     unsigned int len;
{
  char *varref = (char *) alloca (2 + len + 2);
  char *value, *p;
  int argc;
  char **argv;

  /* Get the variable's value.  */
  varref[0] = '$';
  varref[1] = '(';
  bcopy (envar, &varref[2], len);
  varref[2 + len] = ')';
  varref[2 + len + 1] = '\0';
  value = variable_expand (varref);

  /* Skip whitespace, and check for an empty value.  */
  value = next_token (value);
  len = strlen (value);
  if (len == 0)
    return;

  /* Allocate a vector that is definitely big enough.  */
  argv = (char **) alloca ((1 + len + 1) * sizeof (char *));

  /* Allocate a buffer to copy the value into while we split it into words
     and unquote it.  We must use permanent storage for this because
     decode_switches may store pointers into the passed argument words.  */
  p = (char *) xmalloc (2 * len);

  /* getopt will look at the arguments starting at ARGV[1].
     Prepend a spacer word.  */
  argv[0] = 0;
  argc = 1;
  argv[argc] = p;
  while (*value != '\0')
    {
      if (*value == '\\')
	++value;		/* Skip the backslash.  */
      else if (isblank (*value))
	{
	  /* End of the word.  */
	  *p++ = '\0';
	  argv[++argc] = p;
	  do
	    ++value;
	  while (isblank (*value));
	  continue;
	}
      *p++ = *value++;
    }
  *p = '\0';
  argv[++argc] = 0;

  if (argv[1][0] != '-' && index (argv[1], '=') == 0)
    /* The first word doesn't start with a dash and isn't a variable
       definition.  Add a dash and pass it along to decode_switches.  We
       need permanent storage for this in case decode_switches saves
       pointers into the value.  */
    argv[1] = concat ("-", argv[1], "");

  /* Parse those words.  */
  decode_switches (argc, argv, 1);
}

/* Quote the string IN so that it will be interpreted as a single word with
   no magic by the shell; if DOUBLE_DOLLARS is nonzero, also double dollar
   signs to avoid variable expansion in make itself.  Write the result into
   OUT, returning the address of the next character to be written.
   Allocating space for OUT twice the length of IN (thrice if
   DOUBLE_DOLLARS is nonzero) is always sufficient.  */

static char *
quote_as_word (out, in, double_dollars)
     char *out, *in;
     int double_dollars;
{
  while (*in != '\0')
    {
#ifdef VMS
      if (index ("^;'\"*?$<>(){}|&~`\\ \t\r\n\f\v", *in) != 0)
#else
      if (index ("^;'\"*?[]$<>(){}|&~`\\ \t\r\n\f\v", *in) != 0)
#endif
	*out++ = '\\';
      if (double_dollars && *in == '$')
	*out++ = '$';
      *out++ = *in++;
    }

  return out;
}

/* Define the MAKEFLAGS and MFLAGS variables to reflect the settings of the
   command switches.  Include options with args if ALL is nonzero.
   Don't include options with the `no_makefile' flag set if MAKEFILE.  */

static void
define_makeflags (all, makefile)
     int all, makefile;
{
  static const char ref[] = "$(MAKEOVERRIDES)";
  static const char posixref[] = "$(-*-command-variables-*-)";
  register const struct command_switch *cs;
  char *flagstring;
  register char *p;
  unsigned int words;
  struct variable *v;

  /* We will construct a linked list of `struct flag's describing
     all the flags which need to go in MAKEFLAGS.  Then, once we
     know how many there are and their lengths, we can put them all
     together in a string.  */

  struct flag
    {
      struct flag *next;
      const struct command_switch *cs;
      char *arg;
    };
  struct flag *flags = 0;
  unsigned int flagslen = 0;
#define	ADD_FLAG(ARG, LEN) \
  do {									      \
    struct flag *new = (struct flag *) alloca (sizeof (struct flag));	      \
    new->cs = cs;							      \
    new->arg = (ARG);							      \
    new->next = flags;							      \
    flags = new;							      \
    if (new->arg == 0)							      \
      ++flagslen;		/* Just a single flag letter.  */	      \
    else								      \
      flagslen += 1 + 1 + 1 + 1 + 3 * (LEN); /* " -x foo" */		      \
    if (!isalnum (cs->c))						      \
      /* This switch has no single-letter version, so we use the long.  */    \
      flagslen += 2 + strlen (cs->long_name);				      \
  } while (0)

  for (cs = switches; cs->c != '\0'; ++cs)
    if (cs->toenv && (!makefile || !cs->no_makefile))
      switch (cs->type)
	{
	default:
	  abort ();

	case ignore:
	  break;

	case flag: case_coverage = case_coverage;
	case flag_off:
	  if (!*(int *) cs->value_ptr == (cs->type == flag_off)
	      && (cs->default_value == 0
		  || *(int *) cs->value_ptr != *(int *) cs->default_value))
	    ADD_FLAG (0, 0);
	  break;

	case positive_int:
	  if (all)
	    {
	      if ((cs->default_value != 0
		   && (*(unsigned int *) cs->value_ptr
		       == *(unsigned int *) cs->default_value)))
		break;
	      else if (cs->noarg_value != 0
		       && (*(unsigned int *) cs->value_ptr ==
			   *(unsigned int *) cs->noarg_value))
		ADD_FLAG ("", 0); /* Optional value omitted; see below.  */
	      else if (cs->c == 'j')
		/* Special case for `-j'.  */
		ADD_FLAG ("1", 1);
	      else
		{
		  char *buf = (char *) alloca (30);
		  sprintf (buf, "%u", *(unsigned int *) cs->value_ptr);
		  ADD_FLAG (buf, strlen (buf));
		}
	    }
	  break;

#ifndef NO_FLOAT
	case floating:
	  if (all)
	    {
	      if (cs->default_value != 0
		  && (*(double *) cs->value_ptr
		      == *(double *) cs->default_value))
		break;
	      else if (cs->noarg_value != 0
		       && (*(double *) cs->value_ptr
			   == *(double *) cs->noarg_value))
		ADD_FLAG ("", 0); /* Optional value omitted; see below.  */
	      else
		{
		  char *buf = (char *) alloca (100);
		  sprintf (buf, "%g", *(double *) cs->value_ptr);
		  ADD_FLAG (buf, strlen (buf));
		}
	    }
	  break;
#endif

	case string:
	  if (all)
	    {
	      struct stringlist *sl = *(struct stringlist **) cs->value_ptr;
	      if (sl != 0)
		{
		  /* Add the elements in reverse order, because
		     all the flags get reversed below; and the order
		     matters for some switches (like -I).  */
		  register unsigned int i = sl->idx;
		  while (i-- > 0)
		    ADD_FLAG (sl->list[i], strlen (sl->list[i]));
		}
	    }
	  break;
	}

  flagslen += 4 + sizeof posixref; /* Four more for the possible " -- ".  */

#undef	ADD_FLAG

  /* Construct the value in FLAGSTRING.
     We allocate enough space for a preceding dash and trailing null.  */
  flagstring = (char *) alloca (1 + flagslen + 1);
  p = flagstring;
  words = 1;
  *p++ = '-';
  while (flags != 0)
    {
      /* Add the flag letter or name to the string.  */
      if (!isalnum (flags->cs->c))
	{
	  *p++ = '-';
	  strcpy (p, flags->cs->long_name);
	  p += strlen (p);
	}
      else
	*p++ = flags->cs->c;
      if (flags->arg != 0)
	{
	  /* A flag that takes an optional argument which in this case is
	     omitted is specified by ARG being "".  We must distinguish
	     because a following flag appended without an intervening " -"
	     is considered the arg for the first.  */
	  if (flags->arg[0] != '\0')
	    {
	      /* Add its argument too.  */
	      *p++ = !isalnum (flags->cs->c) ? '=' : ' ';
	      p = quote_as_word (p, flags->arg, 1);
	    }
	  ++words;
	  /* Write a following space and dash, for the next flag.  */
	  *p++ = ' ';
	  *p++ = '-';
	}
      else if (!isalnum (flags->cs->c))
	{
	  ++words;
	  /* Long options must each go in their own word,
	     so we write the following space and dash.  */
	  *p++ = ' ';
	  *p++ = '-';
	}
      flags = flags->next;
    }

  /* Define MFLAGS before appending variable definitions.  */

  if (p == &flagstring[1])
    /* No flags.  */
    flagstring[0] = '\0';
  else if (p[-1] == '-')
    {
      /* Kill the final space and dash.  */
      p -= 2;
      *p = '\0';
    }
  else
    /* Terminate the string.  */
    *p = '\0';

  /* Since MFLAGS is not parsed for flags, there is no reason to
     override any makefile redefinition.  */
  (void) define_variable ("MFLAGS", 6, flagstring, o_env, 1);

  if (all && command_variables != 0)
    {
      /* Now write a reference to $(MAKEOVERRIDES), which contains all the
	 command-line variable definitions.  */

      if (p == &flagstring[1])
	/* No flags written, so elide the leading dash already written.  */
	p = flagstring;
      else
	{
	  /* Separate the variables from the switches with a "--" arg.  */
	  if (p[-1] != '-')
	    {
	      /* We did not already write a trailing " -".  */
	      *p++ = ' ';
	      *p++ = '-';
	    }
	  /* There is a trailing " -"; fill it out to " -- ".  */
	  *p++ = '-';
	  *p++ = ' ';
	}

      /* Copy in the string.  */
      if (posix_pedantic)
	{
	  bcopy (posixref, p, sizeof posixref - 1);
	  p += sizeof posixref - 1;
	}
      else
	{
	  bcopy (ref, p, sizeof ref - 1);
	  p += sizeof ref - 1;
	}
    }
  else if (p == &flagstring[1])
    {
      words = 0;
      --p;
    }
  else if (p[-1] == '-')
    /* Kill the final space and dash.  */
    p -= 2;
  /* Terminate the string.  */
  *p = '\0';

  v = define_variable ("MAKEFLAGS", 9,
		       /* If there are switches, omit the leading dash
			  unless it is a single long option with two
			  leading dashes.  */
		       &flagstring[(flagstring[0] == '-'
				    && flagstring[1] != '-')
				   ? 1 : 0],
		       /* This used to use o_env, but that lost when a
			  makefile defined MAKEFLAGS.  Makefiles set
			  MAKEFLAGS to add switches, but we still want
			  to redefine its value with the full set of
			  switches.  Of course, an override or command
			  definition will still take precedence.  */
		       o_file, 1);
  if (! all)
    /* The first time we are called, set MAKEFLAGS to always be exported.
       We should not do this again on the second call, because that is
       after reading makefiles which might have done `unexport MAKEFLAGS'. */
    v->export = v_export;
}

/* Print version information.  */

static void
print_version ()
{
  static int printed_version = 0;

  char *precede = print_data_base_flag ? "# " : "";

  if (printed_version)
    /* Do it only once.  */
    return;

  printf ("%sGNU Make version %s", precede, version_string);
  if (remote_description != 0 && *remote_description != '\0')
    printf ("-%s", remote_description);

  printf (", by Richard Stallman and Roland McGrath.\n\
%sCopyright (C) 1988, 89, 90, 91, 92, 93, 94, 95, 96\n\
%s\tFree Software Foundation, Inc.\n\
%sThis is free software; see the source for copying conditions.\n\
%sThere is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A\n\
%sPARTICULAR PURPOSE.\n\n\
%sReport bugs to <bug-gnu-utils@prep.ai.mit.edu>.\n\n",
	  precede, precede, precede, precede, precede, precede);

  printed_version = 1;

  /* Flush stdout so the user doesn't have to wait to see the
     version information while things are thought about.  */
  fflush (stdout);
}

/* Print a bunch of information about this and that.  */

static void
print_data_base ()
{
  time_t when;

  when = time ((time_t *) 0);

  /* printf ("\n# Make data base, printed on %s", ctime (&when)); */
  printf ("\n# Make data base, printed on Sun Jan 01 00:00:00 2000");

  print_variable_data_base ();
  print_dir_data_base ();
  print_rule_data_base ();
  print_file_data_base ();
  print_vpath_data_base ();

  when = time ((time_t *) 0);

  /* printf ("\n# Finished Make data base on %s\n", ctime (&when));*/
  printf ("\n# Finished Make data base on Sun Jan 01 00:00:00 2000");
}

/* Exit with STATUS, cleaning up as necessary.  */

void
die (status)
     int status;
{
  static char dying = 0;

  if (!dying)
    {
      int err;

      dying = 1;

      /* Try to move back to the original directory.  This is essential on
	 MS-DOS (where there is really only one process), and on Unix it
	 puts core files in the original directory instead of the -C
	 directory.  */
      if (directory_before_chdir != 0)
	chdir (directory_before_chdir);

      if (print_version_flag)
	print_version ();

      /* Wait for children to die.  */
      for (err = status != 0; job_slots_used > 0; err = 0)
	reap_children (1, err);

      /* Let the remote job module clean up its state.  */
      remote_cleanup ();

      /* Remove the intermediate files.  */
      remove_intermediates (0);

      if (print_data_base_flag)
	print_data_base ();

      log_working_directory (0);
    }

  exit (status);
}

/* Write a message indicating that we've just entered or
   left (according to ENTERING) the current directory.  */

void
log_working_directory (entering)
     int entering;
{
  static int entered = 0;
  char *message = entering ? "Entering" : "Leaving";

  /* Print nothing without the flag.  Don't print the entering message
     again if we already have.  Don't print the leaving message if we
     haven't printed the entering message.  */
  if (! print_directory_flag || entering == entered)
    return;

  entered = entering;

  if (print_data_base_flag)
    fputs ("# ", stdout);

  if (makelevel == 0)
    printf ("%s: %s ", program, message);
  else
    printf ("%s[%u]: %s ", program, makelevel, message);

  if (starting_directory == 0)
    puts ("an unknown directory");
  else
    printf ("directory `%s'\n", starting_directory);
}



  /*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         read.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Reading and parsing of makefiles for GNU Make.
Copyright (C) 1988,89,90,91,92,93,94,95,96 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "dep.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
#include "job.h"
/* #include "commands.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "variable.h"  <- modification by J.Ruthruff, 7/20 */
#undef stderr
#define stderr stdout

/* This is POSIX.2, but most systems using -DPOSIX probably don't have it.  */
#ifdef	HAVE_GLOB_H
#include <glob.h>
#undef stderr
#define stderr stdout
#else
#include "glob.h"
#undef stderr
#define stderr stdout
#endif

#ifndef WIN32
#ifndef _AMIGA
#ifndef VMS
#include <pwd.h>
#undef stderr
#define stderr stdout
#else
struct passwd *getpwnam PARAMS ((char *name));
#endif
#endif
#endif /* !WIN32 */

/* A `struct linebuffer' is a structure which holds a line of text.
   `readline' reads a line from a stream into a linebuffer
   and works regardless of the length of the line.  */

struct linebuffer
  {
    /* Note:  This is the number of bytes malloc'ed for `buffer'
       It does not indicate `buffer's real length.
       Instead, a null char indicates end-of-string.  */
    unsigned int size;
    char *buffer;
  };

#define initbuffer(lb) (lb)->buffer = (char *) xmalloc ((lb)->size = 200)
#define freebuffer(lb) free ((lb)->buffer)


/* A `struct conditionals' contains the information describing
   all the active conditionals in a makefile.

   The global variable `conditionals' contains the conditionals
   information for the current makefile.  It is initialized from
   the static structure `toplevel_conditionals' and is later changed
   to new structures for included makefiles.  */

struct conditionals
  {
    unsigned int if_cmds;	/* Depth of conditional nesting.  */
    unsigned int allocated;	/* Elts allocated in following arrays.  */
    char *ignoring;		/* Are we ignoring or interepreting?  */
    char *seen_else;		/* Have we already seen an `else'?  */
  };

static struct conditionals toplevel_conditionals;
static struct conditionals *conditionals = &toplevel_conditionals;


/* Default directories to search for include files in  */

static char *default_include_directories[] =
  {
#if defined(WIN32) && !defined(INCLUDEDIR)
/*
 * This completly up to the user when they install MSVC or other packages.
 * This is defined as a placeholder.
 */
#define INCLUDEDIR "/usr/local/include"  /* modification by J.Ruthruff, 8/16 */
#endif
    INCLUDEDIR,
#ifndef _AMIGA
    "/usr/gnu/include",
    "/usr/local/include",
    "/usr/include",
#endif
    0
  };

/* List of directories to search for include files in  */

static char **include_directories_to_search;

/* Maximum length of an element of the above.  */

static unsigned int max_incl_len;

/* The filename and pointer to line number of the
   makefile currently being read in.  */

char *reading_filename;
unsigned int *reading_lineno_ptr;

/* The chain of makefiles read by read_makefile.  */

static struct dep *read_makefiles = 0;

static int read_makefile PARAMS ((char *filename, int flags));
static unsigned int readline PARAMS ((struct linebuffer *linebuffer, FILE *stream,
			char *filename, unsigned int lineno));
static unsigned int do_define PARAMS ((char *name, unsigned int namelen, enum variable_origin origin,
			unsigned int lineno, FILE *infile, char *filename));
static int conditional_line PARAMS ((char *line, char *filename, unsigned int lineno));
static void record_files PARAMS ((struct nameseq *filenames, char *pattern, char *pattern_percent,
			struct dep *deps, unsigned int commands_started, char *commands,
			unsigned int commands_idx, int two_colon, char *filename,
			unsigned int lineno, int set_default));

/* Read in all the makefiles and return the chain of their names.  */

struct dep *
read_all_makefiles (makefiles)
     char **makefiles;
{
  unsigned int num_makefiles = 0;

  if (debug_flag)
    puts ("Reading makefiles...");

  /* If there's a non-null variable MAKEFILES, its value is a list of
     files to read first thing.  But don't let it prevent reading the
     default makefiles and don't let the default goal come from there.  */

  {
    char *value;
    char *name, *p;
    unsigned int length;

    {
      /* Turn off --warn-undefined-variables while we expand MAKEFILES.  */
      int save = warn_undefined_variables_flag;
      warn_undefined_variables_flag = 0;

      value = allocated_variable_expand ("$(MAKEFILES)");

      warn_undefined_variables_flag = save;
    }

    /* Set NAME to the start of next token and LENGTH to its length.
       MAKEFILES is updated for finding remaining tokens.  */
    p = value;

    while ((name = find_next_token (&p, &length)) != 0)
      {
	if (*p != '\0')
	  *p++ = '\0';
	(void) read_makefile (name,
			      RM_NO_DEFAULT_GOAL | RM_INCLUDED | RM_DONTCARE);
      }

    free (value);
  }

  /* Read makefiles specified with -f switches.  */

  if (makefiles != 0)
    while (*makefiles != 0)
      {
	struct dep *tail = read_makefiles;
	register struct dep *d;

	if (! read_makefile (*makefiles, 0))
	  perror_with_name ("", *makefiles);

	/* Find the right element of read_makefiles.  */
	d = read_makefiles;
	while (d->next != tail)
	  d = d->next;

	/* Use the storage read_makefile allocates.  */
	*makefiles = dep_name (d);
	++num_makefiles;
	++makefiles;
      }

  /* If there were no -f switches, try the default names.  */

  if (num_makefiles == 0)
    {
      static char *default_makefiles[] =
#ifdef VMS
	/* all lower case since readdir() (the vms version) 'lowercasifies' */
	{ "makefile.vms", "gnumakefile", "makefile", 0 };
#else
#ifdef _AMIGA
	{ "GNUmakefile", "Makefile", "SMakefile", 0 };
#else /* !Amiga && !VMS */
	{ "GNUmakefile", "makefile", "Makefile", 0 };
#endif /* AMIGA */
#endif /* VMS */
      register char **p = default_makefiles;
      while (*p != 0 && !file_exists_p (*p))
	++p;

      if (*p != 0)
	{
	  if (! read_makefile (*p, 0))
	    perror_with_name ("", *p);
	}
      else
	{
	  /* No default makefile was found.  Add the default makefiles to the
	     `read_makefiles' chain so they will be updated if possible.  */
	  struct dep *tail = read_makefiles;
	  /* Add them to the tail, after any MAKEFILES variable makefiles.  */
	  while (tail != 0 && tail->next != 0)
	    tail = tail->next;
	  for (p = default_makefiles; *p != 0; ++p)
	    {
	      struct dep *d = (struct dep *) xmalloc (sizeof (struct dep));
	      d->name = 0;
	      d->file = enter_file (*p);
	      d->file->dontcare = 1;
	      /* Tell update_goal_chain to bail out as soon as this file is
		 made, and main not to die if we can't make this file.  */
	      d->changed = RM_DONTCARE;
	      if (tail == 0)
		read_makefiles = d;
	      else
		tail->next = d;
	      tail = d;
	    }
	  if (tail != 0)
	    tail->next = 0;
	}
    }

  return read_makefiles;
}

/* Read file FILENAME as a makefile and add its contents to the data base.

   FLAGS contains bits as above.

   FILENAME is added to the `read_makefiles' chain.

   Returns 1 if a file was found and read, 0 if not.  */

static int
read_makefile (filename, flags)
     char *filename;
     int flags;
{
  static char *collapsed = 0;
  static unsigned int collapsed_length = 0;
  register FILE *infile;
  struct linebuffer lb;
  unsigned int commands_len = 200;
  char *commands = (char *) xmalloc (200);
  unsigned int commands_idx = 0;
  unsigned int commands_started;
  register char *p;
  char *p2;
  int ignoring = 0, in_ignored_define = 0;
  int no_targets = 0;		/* Set when reading a rule without targets.  */
  char *passed_filename = filename;

  struct nameseq *filenames = 0;
  struct dep *deps;
  unsigned int lineno = 1;
  unsigned int nlines = 0;
  int two_colon;
  char *pattern = 0, *pattern_percent;

  int makefile_errno;
#ifdef WIN32
  int check_again;
#endif

#define record_waiting_files()						      \
  do									      \
    { 									      \
      if (filenames != 0)						      \
	record_files (filenames, pattern, pattern_percent, deps,	      \
		      commands_started, commands, commands_idx,		      \
		      two_colon, filename, lineno,			      \
		      !(flags & RM_NO_DEFAULT_GOAL));		     	      \
      filenames = 0;							      \
      commands_idx = 0;							      \
      pattern = 0;							      \
    } while (0)

#ifdef	lint	/* Suppress `used before set' messages.  */
  two_colon = 0;
  pattern_percent = 0;
#endif

  if (debug_flag)
    {
      printf ("Reading makefile `%s'", filename);
      if (flags & RM_NO_DEFAULT_GOAL)
	printf (" (no default goal)");
      if (flags & RM_INCLUDED)
	printf (" (search path)");
      if (flags & RM_DONTCARE)
	printf (" (don't care)");
      if (flags & RM_NO_TILDE)
	printf (" (no ~ expansion)");
      puts ("...");
    }

  /* First, get a stream to read.  */

  /* Expand ~ in FILENAME unless it came from `include',
     in which case it was already done.  */
  if (!(flags & RM_NO_TILDE) && filename[0] == '~')
    {
      char *expanded = tilde_expand (filename);
      if (expanded != 0)
	filename = expanded;
    }

  infile = fopen (filename, "r");
  /* Save the error code so we print the right message later.  */
  makefile_errno = errno;

  /* If the makefile wasn't found and it's either a makefile from
     the `MAKEFILES' variable or an included makefile,
     search the included makefile search path for this makefile.  */
  if (infile == 0 && (flags & RM_INCLUDED) && *filename != '/')
    {
      register unsigned int i;
      for (i = 0; include_directories_to_search[i] != 0; ++i)
	{
	  char *name = concat (include_directories_to_search[i], "/", filename);
	  infile = fopen (name, "r");
	  if (infile == 0)
	    free (name);
	  else
	    {
	      filename = name;
	      break;
	    }
	}
    }

  /* Add FILENAME to the chain of read makefiles.  */
  deps = (struct dep *) xmalloc (sizeof (struct dep));
  deps->next = read_makefiles;
  read_makefiles = deps;
  deps->name = 0;
  deps->file = lookup_file (filename);
  if (deps->file == 0)
    {
      deps->file = enter_file (savestring (filename, strlen (filename)));
      if (flags & RM_DONTCARE)
	deps->file->dontcare = 1;
    }
  if (filename != passed_filename)
    free (filename);
  filename = deps->file->name;
  deps->changed = flags;
  deps = 0;

  /* If the makefile can't be found at all, give up entirely.  */

  if (infile == 0)
    {
      /* If we did some searching, errno has the error from the last
	 attempt, rather from FILENAME itself.  Restore it in case the
	 caller wants to use it in a message.  */
      errno = makefile_errno;
      return 0;
    }

  reading_filename = filename;
  reading_lineno_ptr = &lineno;

  /* Loop over lines in the file.
     The strategy is to accumulate target names in FILENAMES, dependencies
     in DEPS and commands in COMMANDS.  These are used to define a rule
     when the start of the next rule (or eof) is encountered.  */

  initbuffer (&lb);

  while (!feof (infile))
    {
      lineno += nlines;
      nlines = readline (&lb, infile, filename, lineno);

      /* Check for a shell command line first.
	 If it is not one, we can stop treating tab specially.  */
      if (lb.buffer[0] == '\t')
	{
	  /* This line is a probably shell command.  */
	  unsigned int len;

	  if (no_targets)
	    /* Ignore the commands in a rule with no targets.  */
	    continue;

	  /* If there is no preceding rule line, don't treat this line
	     as a command, even though it begins with a tab character.
	     SunOS 4 make appears to behave this way.  */

	  if (filenames != 0)
	    {
	      if (ignoring)
		/* Yep, this is a shell command, and we don't care.  */
		continue;

	      /* Append this command line to the line being accumulated.  */
	      p = lb.buffer;
	      if (commands_idx == 0)
		commands_started = lineno;
	      len = strlen (p);
	      if (len + 1 + commands_idx > commands_len)
		{
		  commands_len = (len + 1 + commands_idx) * 2;
		  commands = (char *) xrealloc (commands, commands_len);
		}
	      bcopy (p, &commands[commands_idx], len);
	      commands_idx += len;
	      commands[commands_idx++] = '\n';

	      continue;
	    }
	}

      /* This line is not a shell command line.  Don't worry about tabs.  */

      if (collapsed_length < lb.size)
	{
	  collapsed_length = lb.size;
	  if (collapsed != 0)
	    free (collapsed);
	  collapsed = (char *) xmalloc (collapsed_length);
	}
      strcpy (collapsed, lb.buffer);
      /* Collapse continuation lines.  */
      collapse_continuations (collapsed);
      remove_comments (collapsed);

      /* strncmp is first to avoid dereferencing out into space.  */
#define	word1eq(s, l) 	(!strncmp (s, p, l) \
			 && (p[l] == '\0' || isblank (p[l])))
      p = collapsed;
      while (isspace (*p))
	++p;
      if (*p == '\0')
	/* This line is completely empty.  */
	continue;

      /* We must first check for conditional and `define' directives before
	 ignoring anything, since they control what we will do with
	 following lines.  */

      if (!in_ignored_define
	  && (word1eq ("ifdef", 5) || word1eq ("ifndef", 6)
	      || word1eq ("ifeq", 4) || word1eq ("ifneq", 5)
	      || word1eq ("else", 4) || word1eq ("endif", 5)))
	{
	  int i = conditional_line (p, filename, lineno);
	  if (i >= 0)
	    ignoring = i;
	  else
	    makefile_fatal (filename, lineno,
			    "invalid syntax in conditional");
	  continue;
	}
      else if (word1eq ("endef", 5))
	{
	  if (in_ignored_define)
	    in_ignored_define = 0;
	  else
	    makefile_fatal (filename, lineno, "extraneous `endef'");
	  continue;
	}
      else if (word1eq ("define", 6))
	{
	  if (ignoring)
	    in_ignored_define = 1;
	  else
	    {
	      p2 = next_token (p + 6);
	      /* Let the variable name be the whole rest of the line,
		 with trailing blanks stripped (comments have already been
		 removed), so it could be a complex variable/function
		 reference that might contain blanks.  */
	      p = index (p2, '\0');
	      while (isblank (p[-1]))
		--p;
	      lineno = do_define (p2, p - p2, o_file,
				  lineno, infile, filename);
	    }
	  continue;
	}
      else if (word1eq ("override", 8))
	{
	  p2 = next_token (p + 8);
	  if (p2 == 0)
	    makefile_error (filename, lineno, "empty `override' directive");
	  if (!strncmp (p2, "define", 6) && (isblank (p2[6]) || p2[6] == '\0'))
	    {
	      if (ignoring)
		in_ignored_define = 1;
	      else
		{
		  p2 = next_token (p2 + 6);
		  /* Let the variable name be the whole rest of the line,
		     with trailing blanks stripped (comments have already been
		     removed), so it could be a complex variable/function
		     reference that might contain blanks.  */
		  p = index (p2, '\0');
		  while (isblank (p[-1]))
		    --p;
		  lineno = do_define (p2, p - p2, o_override,
				      lineno, infile, filename);
		}
	    }
	  else if (!ignoring
		   && !try_variable_definition (filename, lineno,
						p2, o_override))
	    makefile_error (filename, lineno, "empty `override' directive");

	  continue;
	}

      if (ignoring)
	/* Ignore the line.  We continue here so conditionals
	   can appear in the middle of a rule.  */
	continue;
      else if (word1eq ("export", 6))
	{
	  struct variable *v;
	  p2 = next_token (p + 6);
	  if (*p2 == '\0')
	    export_all_variables = 1;
	  v = try_variable_definition (filename, lineno, p2, o_file);
	  if (v != 0)
	    v->export = v_export;
	  else
	    {
	      unsigned int len;
	      for (p = find_next_token (&p2, &len); p != 0;
		   p = find_next_token (&p2, &len))
		{
		  v = lookup_variable (p, len);
		  if (v == 0)
		    v = define_variable (p, len, "", o_file, 0);
		  v->export = v_export;
		}
	    }
	}
      else if (word1eq ("unexport", 8))
	{
	  unsigned int len;
	  struct variable *v;
	  p2 = next_token (p + 8);
	  if (*p2 == '\0')
	    export_all_variables = 0;
	  for (p = find_next_token (&p2, &len); p != 0;
	       p = find_next_token (&p2, &len))
	    {
	      v = lookup_variable (p, len);
	      if (v == 0)
		v = define_variable (p, len, "", o_file, 0);
	      v->export = v_noexport;
	    }
	}
      else if (word1eq ("include", 7) || word1eq ("-include", 8)
	       || word1eq ("sinclude", 8))
	{
	  /* We have found an `include' line specifying a nested
	     makefile to be read at this point.  */
	  struct conditionals *save, new_conditionals;
	  struct nameseq *files;
	  /* "-include" (vs "include") says no error if the file does not
	     exist.  "sinclude" is an alias for this from SGI.  */
	  int noerror = p[0] != 'i';

	  p = allocated_variable_expand (next_token (p + (noerror ? 9 : 8)));
	  if (*p == '\0')
	    {
	      makefile_error (filename, lineno,
			      "no file name for `%sinclude'",
			      noerror ? "-" : "");
	      continue;
	    }

	  /* Parse the list of file names.  */
	  p2 = p;
	  files = multi_glob (parse_file_seq (&p2, '\0',
					      sizeof (struct nameseq),
					      1),
			      sizeof (struct nameseq));
	  free (p);

	  /* Save the state of conditionals and start
	     the included makefile with a clean slate.  */
	  save = conditionals;
	  bzero ((char *) &new_conditionals, sizeof new_conditionals);
	  conditionals = &new_conditionals;

	  /* Record the rules that are waiting so they will determine
	     the default goal before those in the included makefile.  */
	  record_waiting_files ();

	  /* Read each included makefile.  */
	  while (files != 0)
	    {
	      struct nameseq *next = files->next;
	      char *name = files->name;
	      free ((char *)files);
	      files = next;

	      if (! read_makefile (name, (RM_INCLUDED | RM_NO_TILDE
					  | (noerror ? RM_DONTCARE : 0)))
		  && ! noerror)
		makefile_error (filename, lineno,
				"%s: %s", name, strerror (errno));
	    }

	  /* Free any space allocated by conditional_line.  */
	  if (conditionals->ignoring)
	    free (conditionals->ignoring);
	  if (conditionals->seen_else)
	    free (conditionals->seen_else);

	  /* Restore state.  */
	  conditionals = save;
	  reading_filename = filename;
	  reading_lineno_ptr = &lineno;
	}
      else if (word1eq ("vpath", 5))
	{
	  char *pattern;
	  unsigned int len;
	  p2 = variable_expand (p + 5);
	  p = find_next_token (&p2, &len);
	  if (p != 0)
	    {
	      pattern = savestring (p, len);
	      p = find_next_token (&p2, &len);
	      /* No searchpath means remove all previous
		 selective VPATH's with the same pattern.  */
	    }
	  else
	    /* No pattern means remove all previous selective VPATH's.  */
	    pattern = 0;
	  construct_vpath_list (pattern, p);
	  if (pattern != 0)
	    free (pattern);
	}
#undef	word1eq
      else if (try_variable_definition (filename, lineno, p, o_file))
	/* This line has been dealt with.  */
	;
      else if (lb.buffer[0] == '\t')
	{
	  p = collapsed;	/* Ignore comments.  */
	  while (isblank (*p))
	    ++p;
	  if (*p == '\0')
	    /* The line is completely blank; that is harmless.  */
	    continue;
	  /* This line starts with a tab but was not caught above
	     because there was no preceding target, and the line
	     might have been usable as a variable definition.
	     But now it is definitely lossage.  */
	  makefile_fatal (filename, lineno,
			  "commands commence before first target");
	}
      else
	{
	  /* This line describes some target files.  */

	  char *cmdleft;

	  /* Record the previous rule.  */

	  record_waiting_files ();

	  /* Search the line for an unquoted ; that is not after an
             unquoted #.  */
	  cmdleft = find_char_unquote (lb.buffer, ";#", 0);
	  if (cmdleft != 0 && *cmdleft == '#')
	    {
	      /* We found a comment before a semicolon.  */
	      *cmdleft = '\0';
	      cmdleft = 0;
	    }
	  else if (cmdleft != 0)
	    /* Found one.  Cut the line short there before expanding it.  */
	    *cmdleft = '\0';

	  collapse_continuations (lb.buffer);

	  /* Expand variable and function references before doing anything
	     else so that special characters can be inside variables.  */
	  p = variable_expand (lb.buffer);

	  if (cmdleft == 0)
	    /* Look for a semicolon in the expanded line.  */
	    cmdleft = find_char_unquote (p, ";", 0);

	  if (cmdleft != 0)
	    /* Cut the line short at the semicolon.  */
	    *cmdleft = '\0';

	  p2 = next_token (p);
	  if (*p2 == '\0')
	    {
	      if (cmdleft != 0)
		makefile_fatal (filename, lineno,
				"missing rule before commands");
	      else
		/* This line contained a variable reference that
		   expanded to nothing but whitespace.  */
		continue;
	    }
	  else if (*p2 == ':')
	    {
	      /* We accept and ignore rules without targets for
		 compatibility with SunOS 4 make.  */
	      no_targets = 1;
	      continue;
	    }

	  filenames = multi_glob (parse_file_seq (&p2, ':',
						  sizeof (struct nameseq),
						  1),
				  sizeof (struct nameseq));
	  if (*p2++ == '\0')
	    makefile_fatal (filename, lineno, "missing separator");
	  /* Is this a one-colon or two-colon entry?  */
	  two_colon = *p2 == ':';
	  if (two_colon)
	    p2++;

	  /* We have some targets, so don't ignore the following commands.  */
	  no_targets = 0;

	  /* Is this a static pattern rule: `target: %targ: %dep; ...'?  */
	  p = index (p2, ':');
	  while (p != 0 && p[-1] == '\\')
	    {
	      register char *q = &p[-1];
	      register int backslash = 0;
	      while (*q-- == '\\')
		backslash = !backslash;
	      if (backslash)
		p = index (p + 1, ':');
	      else
		break;
	    }
#ifdef __MSDOS__
	  /* For MS-DOS, skip a "C:\...".  */
	  if (p != 0 && p[1] == '\\' && isalpha (p[-1]))
	    p = 0;
#endif
#ifdef _AMIGA
	  /* Here, the situation is quite complicated. Let's have a look
	    at a couple of targets:

		install: dev:make

		dev:make: make

		dev:make:: xyz

	    The rule is that it's only a target, if there are TWO :'s
	    OR a space around the :.
	  */
	  if (p && !(isspace(p[1]) || !p[1] || isspace(p[-1])))
	    p = 0;
#endif
#ifdef WIN32
          do {
            check_again = 0;
            /* For WIN32, skip a "C:\..." or a "C:/..." */
            if (p != 0 && (p[1] == '\\' || p[1] == '/') && isalpha (p[-1])) {
              p = index(p + 1, ':');
              check_again = 1;
            }
          } while (check_again);
#endif
	  if (p != 0)
	    {
	      struct nameseq *target;
	      target = parse_file_seq (&p2, ':', sizeof (struct nameseq), 1);
	      ++p2;
	      if (target == 0)
		makefile_fatal (filename, lineno, "missing target pattern");
	      else if (target->next != 0)
		makefile_fatal (filename, lineno, "multiple target patterns");
	      pattern = target->name;
	      pattern_percent = find_percent (pattern);
	      if (pattern_percent == 0)
		makefile_fatal (filename, lineno,
				"target pattern contains no `%%'");
	    }
	  else
	    pattern = 0;

	  /* Parse the dependencies.  */
	  deps = (struct dep *)
	    multi_glob (parse_file_seq (&p2, '\0', sizeof (struct dep), 1),
			sizeof (struct dep));

	  commands_idx = 0;
	  if (cmdleft != 0)
	    {
	      /* Semicolon means rest of line is a command.  */
	      unsigned int len = strlen (cmdleft + 1);

	      commands_started = lineno;

	      /* Add this command line to the buffer.  */
	      if (len + 2 > commands_len)
		{
		  commands_len = (len + 2) * 2;
		  commands = (char *) xrealloc (commands, commands_len);
		}
	      bcopy (cmdleft + 1, commands, len);
	      commands_idx += len;
	      commands[commands_idx++] = '\n';
	    }

	  continue;
	}

      /* We get here except in the case that we just read a rule line.
	 Record now the last rule we read, so following spurious
	 commands are properly diagnosed.  */
      record_waiting_files ();
      no_targets = 0;
    }

  if (conditionals->if_cmds)
    makefile_fatal (filename, lineno, "missing `endif'");

  /* At eof, record the last rule.  */
  record_waiting_files ();

  freebuffer (&lb);
  free ((char *) commands);
  fclose (infile);

  reading_filename = 0;
  reading_lineno_ptr = 0;

  return 1;
}

/* Execute a `define' directive.
   The first line has already been read, and NAME is the name of
   the variable to be defined.  The following lines remain to be read.
   LINENO, INFILE and FILENAME refer to the makefile being read.
   The value returned is LINENO, updated for lines read here.  */

static unsigned int
do_define (name, namelen, origin, lineno, infile, filename)
     char *name;
     unsigned int namelen;
     enum variable_origin origin;
     unsigned int lineno;
     FILE *infile;
     char *filename;
{
  struct linebuffer lb;
  unsigned int nlines = 0;
  unsigned int length = 100;
  char *definition = (char *) xmalloc (100);
  register unsigned int idx = 0;
  register char *p;

  /* Expand the variable name.  */
  char *var = (char *) alloca (namelen + 1);
  bcopy (name, var, namelen);
  var[namelen] = '\0';
  var = variable_expand (var);

  initbuffer (&lb);
  while (!feof (infile))
    {
      lineno += nlines;
      nlines = readline (&lb, infile, filename, lineno);

      collapse_continuations (lb.buffer);

      p = next_token (lb.buffer);
      if ((p[5] == '\0' || isblank (p[5])) && !strncmp (p, "endef", 5))
	{
	  p += 5;
	  remove_comments (p);
	  if (*next_token (p) != '\0')
	    makefile_error (filename, lineno,
			    "Extraneous text after `endef' directive");
	  /* Define the variable.  */
	  if (idx == 0)
	    definition[0] = '\0';
	  else
	    definition[idx - 1] = '\0';
	  (void) define_variable (var, strlen (var), definition, origin, 1);
	  free (definition);
	  freebuffer (&lb);
	  return lineno;
	}
      else
	{
	  unsigned int len = strlen (lb.buffer);

	  /* Increase the buffer size if necessary.  */
	  if (idx + len + 1 > length)
	    {
	      length = (idx + len) * 2;
	      definition = (char *) xrealloc (definition, length + 1);
	    }

	  bcopy (lb.buffer, &definition[idx], len);
	  idx += len;
	  /* Separate lines with a newline.  */
	  definition[idx++] = '\n';
	}
    }

  /* No `endef'!!  */
  makefile_fatal (filename, lineno, "missing `endef', unterminated `define'");

  /* NOTREACHED */
  return 0;
}

/* Interpret conditional commands "ifdef", "ifndef", "ifeq",
   "ifneq", "else" and "endif".
   LINE is the input line, with the command as its first word.

   FILENAME and LINENO are the filename and line number in the
   current makefile.  They are used for error messages.

   Value is -1 if the line is invalid,
   0 if following text should be interpreted,
   1 if following text should be ignored.  */

static int
conditional_line (line, filename, lineno)
     char *line;
     char *filename;
     unsigned int lineno;
{
  int notdef;
  char *cmdname;
  register unsigned int i;

  if (*line == 'i')
    {
      /* It's an "if..." command.  */
      notdef = line[2] == 'n';
      if (notdef)
	{
	  cmdname = line[3] == 'd' ? "ifndef" : "ifneq";
	  line += cmdname[3] == 'd' ? 7 : 6;
	}
      else
	{
	  cmdname = line[2] == 'd' ? "ifdef" : "ifeq";
	  line += cmdname[2] == 'd' ? 6 : 5;
	}
    }
  else
    {
      /* It's an "else" or "endif" command.  */
      notdef = line[1] == 'n';
      cmdname = notdef ? "endif" : "else";
      line += notdef ? 5 : 4;
    }

  line = next_token (line);

  if (*cmdname == 'e')
    {
      if (*line != '\0')
	makefile_error (filename, lineno,
			"Extraneous text after `%s' directive",
			cmdname);
      /* "Else" or "endif".  */
      if (conditionals->if_cmds == 0)
	makefile_fatal (filename, lineno, "extraneous `%s'", cmdname);
      /* NOTDEF indicates an `endif' command.  */
      if (notdef)
	--conditionals->if_cmds;
      else if (conditionals->seen_else[conditionals->if_cmds - 1])
	makefile_fatal (filename, lineno, "only one `else' per conditional");
      else
	{
	  /* Toggle the state of ignorance.  */
	  conditionals->ignoring[conditionals->if_cmds - 1]
	    = !conditionals->ignoring[conditionals->if_cmds - 1];
	  /* Record that we have seen an `else' in this conditional.
	     A second `else' will be erroneous.  */
	  conditionals->seen_else[conditionals->if_cmds - 1] = 1;
	}
      for (i = 0; i < conditionals->if_cmds; ++i)
	if (conditionals->ignoring[i])
	  return 1;
      return 0;
    }

  if (conditionals->allocated == 0)
    {
      conditionals->allocated = 5;
      conditionals->ignoring = (char *) xmalloc (conditionals->allocated);
      conditionals->seen_else = (char *) xmalloc (conditionals->allocated);
    }

  ++conditionals->if_cmds;
  if (conditionals->if_cmds > conditionals->allocated)
    {
      conditionals->allocated += 5;
      conditionals->ignoring = (char *)
	xrealloc (conditionals->ignoring, conditionals->allocated);
      conditionals->seen_else = (char *)
	xrealloc (conditionals->seen_else, conditionals->allocated);
    }

  /* Record that we have seen an `if...' but no `else' so far.  */
  conditionals->seen_else[conditionals->if_cmds - 1] = 0;

  /* Search through the stack to see if we're already ignoring.  */
  for (i = 0; i < conditionals->if_cmds - 1; ++i)
    if (conditionals->ignoring[i])
      {
	/* We are already ignoring, so just push a level
	   to match the next "else" or "endif", and keep ignoring.
	   We don't want to expand variables in the condition.  */
	conditionals->ignoring[conditionals->if_cmds - 1] = 1;
	return 1;
      }

  if (cmdname[notdef ? 3 : 2] == 'd')
    {
      /* "Ifdef" or "ifndef".  */
      struct variable *v;
      register char *p = end_of_token (line);
      i = p - line;
      p = next_token (p);
      if (*p != '\0')
	return -1;
      v = lookup_variable (line, i);
      conditionals->ignoring[conditionals->if_cmds - 1]
	= (v != 0 && *v->value != '\0') == notdef;
    }
  else
    {
      /* "Ifeq" or "ifneq".  */
      char *s1, *s2;
      unsigned int len;
      char termin = *line == '(' ? ',' : *line;

      if (termin != ',' && termin != '"' && termin != '\'')
	return -1;

      s1 = ++line;
      /* Find the end of the first string.  */
      if (termin == ',')
	{
	  register int count = 0;
	  for (; *line != '\0'; ++line)
	    if (*line == '(')
	      ++count;
	    else if (*line == ')')
	      --count;
	    else if (*line == ',' && count <= 0)
	      break;
	}
      else
	while (*line != '\0' && *line != termin)
	  ++line;

      if (*line == '\0')
	return -1;

      if (termin == ',')
	{
	  /* Strip blanks after the first string.  */
	  char *p = line++;
	  while (isblank (p[-1]))
	    --p;
	  *p = '\0';
	}
      else
	*line++ = '\0';

      s2 = variable_expand (s1);
      /* We must allocate a new copy of the expanded string because
	 variable_expand re-uses the same buffer.  */
      len = strlen (s2);
      s1 = (char *) alloca (len + 1);
      bcopy (s2, s1, len + 1);

      if (termin != ',')
	/* Find the start of the second string.  */
	line = next_token (line);

      termin = termin == ',' ? ')' : *line;
      if (termin != ')' && termin != '"' && termin != '\'')
	return -1;

      /* Find the end of the second string.  */
      if (termin == ')')
	{
	  register int count = 0;
	  s2 = next_token (line);
	  for (line = s2; *line != '\0'; ++line)
	    {
	      if (*line == '(')
		++count;
	      else if (*line == ')')
		if (count <= 0)
		  break;
		else
		  --count;
	    }
	}
      else
	{
	  ++line;
	  s2 = line;
	  while (*line != '\0' && *line != termin)
	    ++line;
	}

      if (*line == '\0')
	return -1;

      *line = '\0';
      line = next_token (++line);
      if (*line != '\0')
	makefile_error (filename, lineno,
			"Extraneous text after `%s' directive",
			cmdname);

      s2 = variable_expand (s2);
      conditionals->ignoring[conditionals->if_cmds - 1]
	= streq (s1, s2) == notdef;
    }

  /* Search through the stack to see if we're ignoring.  */
  for (i = 0; i < conditionals->if_cmds; ++i)
    if (conditionals->ignoring[i])
      return 1;
  return 0;
}

/* Remove duplicate dependencies in CHAIN.  */

void
uniquize_deps (chain)
     struct dep *chain;
{
  register struct dep *d;

  /* Make sure that no dependencies are repeated.  This does not
     really matter for the purpose of updating targets, but it
     might make some names be listed twice for $^ and $?.  */

  for (d = chain; d != 0; d = d->next)
    {
      struct dep *last, *next;

      last = d;
      next = d->next;
      while (next != 0)
	if (streq (dep_name (d), dep_name (next)))
	  {
	    struct dep *n = next->next;
	    last->next = n;
	    if (next->name != 0 && next->name != d->name)
	      free (next->name);
	    if (next != d)
	      free ((char *) next);
	    next = n;
	  }
	else
	  {
	    last = next;
	    next = next->next;
	  }
    }
}

/* Record a description line for files FILENAMES,
   with dependencies DEPS, commands to execute described
   by COMMANDS and COMMANDS_IDX, coming from FILENAME:COMMANDS_STARTED.
   TWO_COLON is nonzero if a double colon was used.
   If not nil, PATTERN is the `%' pattern to make this
   a static pattern rule, and PATTERN_PERCENT is a pointer
   to the `%' within it.

   The links of FILENAMES are freed, and so are any names in it
   that are not incorporated into other data structures.  */

static void
record_files (filenames, pattern, pattern_percent, deps, commands_started,
	      commands, commands_idx, two_colon, filename, lineno, set_default)
     struct nameseq *filenames;
     char *pattern, *pattern_percent;
     struct dep *deps;
     unsigned int commands_started;
     char *commands;
     unsigned int commands_idx;
     int two_colon;
     char *filename;
     unsigned int lineno;
     int set_default;
{
  struct nameseq *nextf;
  int implicit = 0;
  unsigned int max_targets, target_idx;
  char **targets = 0, **target_percents = 0;
  struct commands *cmds;

  if (commands_idx > 0)
    {
      cmds = (struct commands *) xmalloc (sizeof (struct commands));
      cmds->filename = filename;
      cmds->lineno = commands_started;
      cmds->commands = savestring (commands, commands_idx);
      cmds->command_lines = 0;
    }
  else
    cmds = 0;

  for (; filenames != 0; filenames = nextf)
    {
      register char *name = filenames->name;
      register struct file *f;
      register struct dep *d;
      struct dep *this;
      char *implicit_percent;

      nextf = filenames->next;
      free ((char *) filenames);

      implicit_percent = find_percent (name);
      implicit |= implicit_percent != 0;

      if (implicit && pattern != 0)
	makefile_fatal (filename, lineno,
			"mixed implicit and static pattern rules");

      if (implicit && implicit_percent == 0)
	makefile_fatal (filename, lineno, "mixed implicit and normal rules");

      if (implicit)
	{
	  if (targets == 0)
	    {
	      max_targets = 5;
	      targets = (char **) xmalloc (5 * sizeof (char *));
	      target_percents = (char **) xmalloc (5 * sizeof (char *));
	      target_idx = 0;
	    }
	  else if (target_idx == max_targets - 1)
	    {
	      max_targets += 5;
	      targets = (char **) xrealloc ((char *) targets,
					    max_targets * sizeof (char *));
	      target_percents
		= (char **) xrealloc ((char *) target_percents,
				      max_targets * sizeof (char *));
	    }
	  targets[target_idx] = name;
	  target_percents[target_idx] = implicit_percent;
	  ++target_idx;
	  continue;
	}

      /* If there are multiple filenames, copy the chain DEPS
	 for all but the last one.  It is not safe for the same deps
	 to go in more than one place in the data base.  */
      this = nextf != 0 ? copy_dep_chain (deps) : deps;

      if (pattern != 0)
	/* If this is an extended static rule:
	   `targets: target%pattern: dep%pattern; cmds',
	   translate each dependency pattern into a plain filename
	   using the target pattern and this target's name.  */
	if (!pattern_matches (pattern, pattern_percent, name))
	  {
	    /* Give a warning if the rule is meaningless.  */
	    makefile_error (filename, lineno,
			    "target `%s' doesn't match the target pattern",
			    name);
	    this = 0;
	  }
	else
	  {
	    /* We use patsubst_expand to do the work of translating
	       the target pattern, the target's name and the dependencies'
	       patterns into plain dependency names.  */
	    char *buffer = variable_expand ("");

	    for (d = this; d != 0; d = d->next)
	      {
		char *o;
		char *percent = find_percent (d->name);
		if (percent == 0)
		  continue;
		o = patsubst_expand (buffer, name, pattern, d->name,
				     pattern_percent, percent);
		free (d->name);
		d->name = savestring (buffer, o - buffer);
	      }
	  }

      if (!two_colon)
	{
	  /* Single-colon.  Combine these dependencies
	     with others in file's existing record, if any.  */
	  f = enter_file (name);

	  if (f->double_colon)
	    makefile_fatal (filename, lineno,
			    "target file `%s' has both : and :: entries",
			    f->name);

	  /* If CMDS == F->CMDS, this target was listed in this rule
	     more than once.  Just give a warning since this is harmless.  */
	  if (cmds != 0 && cmds == f->cmds)
	    makefile_error
	      (filename, lineno,
	       "target `%s' given more than once in the same rule.",
	       f->name);

	  /* Check for two single-colon entries both with commands.
	     Check is_target so that we don't lose on files such as .c.o
	     whose commands were preinitialized.  */
	  else if (cmds != 0 && f->cmds != 0 && f->is_target)
	    {
	      makefile_error (cmds->filename, cmds->lineno,
			      "warning: overriding commands for target `%s'",
			      f->name);
	      makefile_error (f->cmds->filename, f->cmds->lineno,
			      "warning: ignoring old commands for target `%s'",
			      f->name);
	    }

	  f->is_target = 1;

	  /* Defining .DEFAULT with no deps or cmds clears it.  */
	  if (f == default_file && this == 0 && cmds == 0)
	    f->cmds = 0;
	  if (cmds != 0)
	    f->cmds = cmds;
	  /* Defining .SUFFIXES with no dependencies
	     clears out the list of suffixes.  */
	  if (f == suffix_file && this == 0)
	    {
	      d = f->deps;
	      while (d != 0)
		{
		  struct dep *nextd = d->next;
 		  free (d->name);
 		  free ((char *)d);
		  d = nextd;
		}
	      f->deps = 0;
	    }
	  else if (f->deps != 0)
	    {
	      /* Add the file's old deps and the new ones in THIS together.  */

	      struct dep *firstdeps, *moredeps;
	      if (cmds != 0)
		{
		  /* This is the rule with commands, so put its deps first.
		     The rationale behind this is that $< expands to the
		     first dep in the chain, and commands use $< expecting
		     to get the dep that rule specifies.  */
		  firstdeps = this;
		  moredeps = f->deps;
		}
	      else
		{
		  /* Append the new deps to the old ones.  */
		  firstdeps = f->deps;
		  moredeps = this;
		}

	      if (firstdeps == 0)
		firstdeps = moredeps;
	      else
		{
		  d = firstdeps;
		  while (d->next != 0)
		    d = d->next;
		  d->next = moredeps;
		}

	      f->deps = firstdeps;
	    }
	  else
	    f->deps = this;

	  /* If this is a static pattern rule, set the file's stem to
	     the part of its name that matched the `%' in the pattern,
	     so you can use $* in the commands.  */
	  if (pattern != 0)
	    {
	      static char *percent = "%";
	      char *buffer = variable_expand ("");
	      char *o = patsubst_expand (buffer, name, pattern, percent,
					 pattern_percent, percent);
	      f->stem = savestring (buffer, o - buffer);
	    }
	}
      else
	{
	  /* Double-colon.  Make a new record
	     even if the file already has one.  */
	  f = lookup_file (name);
	  /* Check for both : and :: rules.  Check is_target so
	     we don't lose on default suffix rules or makefiles.  */
	  if (f != 0 && f->is_target && !f->double_colon)
	    makefile_fatal (filename, lineno,
			    "target file `%s' has both : and :: entries",
			    f->name);
	  f = enter_file (name);
	  /* If there was an existing entry and it was a double-colon
	     entry, enter_file will have returned a new one, making it the
	     prev pointer of the old one, and setting its double_colon
	     pointer to the first one.  */
	  if (f->double_colon == 0)
	    /* This is the first entry for this name, so we must
	       set its double_colon pointer to itself.  */
	    f->double_colon = f;
	  f->is_target = 1;
	  f->deps = this;
	  f->cmds = cmds;
	}

      /* Free name if not needed further.  */
      if (f != 0 && name != f->name
	  && (name < f->name || name > f->name + strlen (f->name)))
	{
	  free (name);
	  name = f->name;
	}

      /* See if this is first target seen whose name does
	 not start with a `.', unless it contains a slash.  */
      if (default_goal_file == 0 && set_default
	  && (*name != '.' || index (name, '/') != 0))
	{
	  int reject = 0;

	  /* If this file is a suffix, don't
	     let it be the default goal file.  */

	  for (d = suffix_file->deps; d != 0; d = d->next)
	    {
	      register struct dep *d2;
	      if (*dep_name (d) != '.' && streq (name, dep_name (d)))
		{
		  reject = 1;
		  break;
		}
	      for (d2 = suffix_file->deps; d2 != 0; d2 = d2->next)
		{
		  register unsigned int len = strlen (dep_name (d2));
		  if (strncmp (name, dep_name (d2), len))
		    continue;
		  if (streq (name + len, dep_name (d)))
		    {
		      reject = 1;
		      break;
		    }
		}
	      if (reject)
		break;
	    }

	  if (!reject)
	    default_goal_file = f;
	}
    }

  if (implicit)
    {
      targets[target_idx] = 0;
      target_percents[target_idx] = 0;
      create_pattern_rule (targets, target_percents, two_colon, deps, cmds, 1);
      free ((char *) target_percents);
    }
}

/* Search STRING for an unquoted STOPCHAR or blank (if BLANK is nonzero).
   Backslashes quote STOPCHAR, blanks if BLANK is nonzero, and backslash.
   Quoting backslashes are removed from STRING by compacting it into
   itself.  Returns a pointer to the first unquoted STOPCHAR if there is
   one, or nil if there are none.  */

char *
find_char_unquote (string, stopchars, blank)
     char *string;
     char *stopchars;
     int blank;
{
  unsigned int string_len = 0;
  register char *p = string;

  while (1)
    {
      while (*p != '\0' && index (stopchars, *p) == 0
	     && (!blank || !isblank (*p)))
	++p;
      if (*p == '\0')
	break;

      if (p > string && p[-1] == '\\')
	{
	  /* Search for more backslashes.  */
	  register int i = -2;
	  while (&p[i] >= string && p[i] == '\\')
	    --i;
	  ++i;
	  /* Only compute the length if really needed.  */
	  if (string_len == 0)
	    string_len = strlen (string);
	  /* The number of backslashes is now -I.
	     Copy P over itself to swallow half of them.  */
	  bcopy (&p[i / 2], &p[i], (string_len - (p - string)) - (i / 2) + 1);
	  p += i / 2;
	  if (i % 2 == 0)
	    /* All the backslashes quoted each other; the STOPCHAR was
	       unquoted.  */
	    return p;

	  /* The STOPCHAR was quoted by a backslash.  Look for another.  */
	}
      else
	/* No backslash in sight.  */
	return p;
    }

  /* Never hit a STOPCHAR or blank (with BLANK nonzero).  */
  return 0;
}

/* Search PATTERN for an unquoted %.  */

char *
find_percent (pattern)
     char *pattern;
{
  return find_char_unquote (pattern, "%", 0);
}

/* Parse a string into a sequence of filenames represented as a
   chain of struct nameseq's in reverse order and return that chain.

   The string is passed as STRINGP, the address of a string pointer.
   The string pointer is updated to point at the first character
   not parsed, which either is a null char or equals STOPCHAR.

   SIZE is how big to construct chain elements.
   This is useful if we want them actually to be other structures
   that have room for additional info.

   If STRIP is nonzero, strip `./'s off the beginning.  */

struct nameseq *
parse_file_seq (stringp, stopchar, size, strip)
     char **stringp;
     int stopchar;
     unsigned int size;
     int strip;
{
  register struct nameseq *new = 0;
  register struct nameseq *new1, *lastnew1;
  register char *p = *stringp;
  char *q;
  char *name;
  char stopchars[3];

#ifdef VMS
  stopchars[0] = ',';
  stopchars[1] = stopchar;
  stopchars[2] = '\0';
#else
  stopchars[0] = stopchar;
  stopchars[1] = '\0';
#endif

  while (1)
    {
      /* Skip whitespace; see if any more names are left.  */
      p = next_token (p);
      if (*p == '\0')
	break;
      if (*p == stopchar)
	break;

      /* Yes, find end of next name.  */
      q = p;
      p = find_char_unquote (q, stopchars, 1);
#ifdef VMS
	/* convert comma separated list to space separated */
      if (p && *p == ',')
	*p =' ';
#endif
#ifdef __MSDOS__
      /* For MS-DOS, skip a "C:\...".  */
      if (stopchar == ':' && p != 0 && p[1] == '\\' && isalpha (p[-1]))
	p = 0;
#endif
#ifdef _AMIGA
      if (stopchar == ':' && p && *p == ':' &&
	!(isspace(p[1]) || !p[1] || isspace(p[-1])))
      {
	p = find_char_unquote (p+1, stopchars, 1);
      }
#endif
#ifdef WIN32
      /* For WIN32, skip a "C:\..." or "C:/...". */
      if (stopchar == ':' &&
          p != 0 &&
          (p[1] == '\\' || p[1] == '/') &&
          isalpha (p[-1])) {
        p = end_of_token_w32(++p, ':');
        if (*p == '\0' && p[-1] == ':')
          p--;
      }
#endif
      if (p == 0)
	p = q + strlen (q);

      if (strip)
#ifdef VMS
	/* Skip leading `[]'s.  */
	while (p - q > 2 && q[0] == '[' && q[1] == ']')
#else
	/* Skip leading `./'s.  */
	while (p - q > 2 && q[0] == '.' && q[1] == '/')
#endif
	  {
	    q += 2;		/* Skip "./".  */
	    while (q < p && *q == '/')
	      /* Skip following slashes: ".//foo" is "foo", not "/foo".  */
	      ++q;
	  }

      /* Extract the filename just found, and skip it.  */

      if (q == p)
	/* ".///" was stripped to "". */
#ifdef VMS
	continue;
#else
#ifdef _AMIGA
	name = savestring ("", 0);
#else
	name = savestring ("./", 2);
#endif
#endif
      else
#ifdef VMS
/* VMS filenames can have a ':' in them but they have to be '\'ed but we need
 *  to remove this '\' before we can use the filename.
 * Savestring called because q may be read-only string constant.
 */
	{
	  char *qbase = savestring(q, strlen(q));
	  char *pbase = qbase + (p-q);
	  char *q1 = qbase;
	  char *q2 = q1;
	  char *p1 = pbase;

	  while (q1 != pbase)
	    {
	      if (*q1 == '\\' && *(q1+1) == ':')
		{
		  q1++;
		  p1--;
		}
	      *q2++ = *q1++;
	    }
	  name = savestring (qbase, p1 - qbase);
	  free (qbase);
	}
#else
	name = savestring (q, p - q);
#endif

      /* Add it to the front of the chain.  */
      new1 = (struct nameseq *) xmalloc (size);
      new1->name = name;
      new1->next = new;
      new = new1;
    }

#ifndef NO_ARCHIVES

  /* Look for multi-word archive references.
     They are indicated by a elt ending with an unmatched `)' and
     an elt further down the chain (i.e., previous in the file list)
     with an unmatched `(' (e.g., "lib(mem").  */

  new1 = new;
  lastnew1 = 0;
  while (new1 != 0)
    if (new1->name[0] != '('	/* Don't catch "(%)" and suchlike.  */
	&& new1->name[strlen (new1->name) - 1] == ')'
	&& index (new1->name, '(') == 0)
      {
	/* NEW1 ends with a `)' but does not contain a `('.
	   Look back for an elt with an opening `(' but no closing `)'.  */

	struct nameseq *n = new1->next, *lastn = new1;
	char *paren;
	while (n != 0 && (paren = index (n->name, '(')) == 0)
	  {
	    lastn = n;
	    n = n->next;
	  }
	if (n != 0
	    /* Ignore something starting with `(', as that cannot actually
	       be an archive-member reference (and treating it as such
	       results in an empty file name, which causes much lossage).  */
	    && n->name[0] != '(')
	  {
	    /* N is the first element in the archive group.
	       Its name looks like "lib(mem" (with no closing `)').  */

	    char *libname;

	    /* Copy "lib(" into LIBNAME.  */
	    ++paren;
	    libname = (char *) alloca (paren - n->name + 1);
	    bcopy (n->name, libname, paren - n->name);
	    libname[paren - n->name] = '\0';

	    if (*paren == '\0')
	      {
		/* N was just "lib(", part of something like "lib( a b)".
		   Edit it out of the chain and free its storage.  */
		lastn->next = n->next;
		free (n->name);
		free ((char *) n);
		/* LASTN->next is the new stopping elt for the loop below.  */
		n = lastn->next;
	      }
	    else
	      {
		/* Replace N's name with the full archive reference.  */
		name = concat (libname, paren, ")");
		free (n->name);
		n->name = name;
	      }

	    if (new1->name[1] == '\0')
	      {
		/* NEW1 is just ")", part of something like "lib(a b )".
		   Omit it from the chain and free its storage.  */
		if (lastnew1 == 0)
		  new = new1->next;
		else
		  lastnew1->next = new1->next;
		lastn = new1;
		new1 = new1->next;
		free (lastn->name);
		free ((char *) lastn);
	      }
	    else
	      {
		/* Replace also NEW1->name, which already has closing `)'.  */
		name = concat (libname, new1->name, "");
		free (new1->name);
		new1->name = name;
		new1 = new1->next;
	      }

	    /* Trace back from NEW1 (the end of the list) until N
	       (the beginning of the list), rewriting each name
	       with the full archive reference.  */

	    while (new1 != n)
	      {
		name = concat (libname, new1->name, ")");
		free (new1->name);
		new1->name = name;
		lastnew1 = new1;
		new1 = new1->next;
	      }
	  }
	else
	  {
	    /* No frobnication happening.  Just step down the list.  */
	    lastnew1 = new1;
	    new1 = new1->next;
	  }
      }
    else
      {
	lastnew1 = new1;
	new1 = new1->next;
      }

#endif

  *stringp = p;
  return new;
}

/* Read a line of text from STREAM into LINEBUFFER.
   Combine continuation lines into one line.
   Return the number of actual lines read (> 1 if hacked continuation lines).
 */

static unsigned int
readline (linebuffer, stream, filename, lineno)
     struct linebuffer *linebuffer;
     FILE *stream;
     char *filename;
     unsigned int lineno;
{
  char *buffer = linebuffer->buffer;
  register char *p = linebuffer->buffer;
  register char *end = p + linebuffer->size;
  register int len, lastlen = 0;
  register char *p2;
  register unsigned int nlines = 0;
  register int backslash;

  *p = '\0';

  while (fgets (p, end - p, stream) != 0)
    {
      len = strlen (p);
      if (len == 0)
	{
	  /* This only happens when the first thing on the line is a '\0'.
	     It is a pretty hopeless case, but (wonder of wonders) Athena
	     lossage strikes again!  (xmkmf puts NULs in its makefiles.)
	     There is nothing really to be done; we synthesize a newline so
	     the following line doesn't appear to be part of this line.  */
	  makefile_error (filename, lineno,
			  "warning: NUL character seen; rest of line ignored");
	  p[0] = '\n';
	  len = 1;
	}

      p += len;
      if (p[-1] != '\n')
	{
	  /* Probably ran out of buffer space.  */
	  register unsigned int p_off = p - buffer;
	  linebuffer->size *= 2;
	  buffer = (char *) xrealloc (buffer, linebuffer->size);
	  p = buffer + p_off;
	  end = buffer + linebuffer->size;
	  linebuffer->buffer = buffer;
	  *p = '\0';
	  lastlen = len;
	  continue;
	}

      ++nlines;

      if (len == 1 && p > buffer)
	/* P is pointing at a newline and it's the beginning of
	   the buffer returned by the last fgets call.  However,
	   it is not necessarily the beginning of a line if P is
	   pointing past the beginning of the holding buffer.
	   If the buffer was just enlarged (right before the newline),
	   we must account for that, so we pretend that the two lines
	   were one line.  */
	len += lastlen;
      lastlen = len;
      backslash = 0;
      for (p2 = p - 2; --len > 0; --p2)
	{
	  if (*p2 == '\\')
	    backslash = !backslash;
	  else
	    break;
	}

      if (!backslash)
	{
	  p[-1] = '\0';
	  break;
	}

      if (end - p <= 1)
	{
	  /* Enlarge the buffer.  */
	  register unsigned int p_off = p - buffer;
	  linebuffer->size *= 2;
	  buffer = (char *) xrealloc (buffer, linebuffer->size);
	  p = buffer + p_off;
	  end = buffer + linebuffer->size;
	  linebuffer->buffer = buffer;
	}
    }

  if (ferror (stream))
    pfatal_with_name (filename);

  return nlines;
}

/* Construct the list of include directories
   from the arguments and the default list.  */

void
construct_include_path (arg_dirs)
     char **arg_dirs;
{
  register unsigned int i;
#ifdef VAXC		/* just don't ask ... */
  stat_t stbuf;
#else
  struct stat stbuf;
#endif
  /* Table to hold the dirs.  */

  register unsigned int defsize = (sizeof (default_include_directories)
				   / sizeof (default_include_directories[0]));
  register unsigned int max = 5;
  register char **dirs = (char **) xmalloc ((5 + defsize) * sizeof (char *));
  register unsigned int idx = 0;

  /* First consider any dirs specified with -I switches.
     Ignore dirs that don't exist.  */

  if (arg_dirs != 0)
    while (*arg_dirs != 0)
      {
	char *dir = *arg_dirs++;

	if (dir[0] == '~')
	  {
	    char *expanded = tilde_expand (dir);
	    if (expanded != 0)
	      dir = expanded;
	  }

	if (stat (dir, &stbuf) == 0 && S_ISDIR (stbuf.st_mode))
	  {
	    if (idx == max - 1)
	      {
		max += 5;
		dirs = (char **)
		  xrealloc ((char *) dirs, (max + defsize) * sizeof (char *));
	      }
	    dirs[idx++] = dir;
	  }
	else if (dir != arg_dirs[-1])
	  free (dir);
      }

  /* Now add at the end the standard default dirs.  */

  for (i = 0; default_include_directories[i] != 0; ++i)
    if (stat (default_include_directories[i], &stbuf) == 0
	&& S_ISDIR (stbuf.st_mode))
      dirs[idx++] = default_include_directories[i];

  dirs[idx] = 0;

  /* Now compute the maximum length of any name in it.  */

  max_incl_len = 0;
  for (i = 0; i < idx; ++i)
    {
      unsigned int len = strlen (dirs[i]);
      /* If dir name is written with a trailing slash, discard it.  */
      if (dirs[i][len - 1] == '/')
	/* We can't just clobber a null in because it may have come from
	   a literal string and literal strings may not be writable.  */
	dirs[i] = savestring (dirs[i], len - 1);
      if (len > max_incl_len)
	max_incl_len = len;
    }

  include_directories_to_search = dirs;
}

/* Expand ~ or ~USER at the beginning of NAME.
   Return a newly malloc'd string or 0.  */

char *
tilde_expand (name)
     char *name;
{
#ifndef VMS
  if (name[1] == '/' || name[1] == '\0')
    {
      extern char *getenv ();
      char *home_dir;
      int is_variable;

      {
	/* Turn off --warn-undefined-variables while we expand HOME.  */
	int save = warn_undefined_variables_flag;
	warn_undefined_variables_flag = 0;

	home_dir = allocated_variable_expand ("$(HOME)");

	warn_undefined_variables_flag = save;
      }

      is_variable = home_dir[0] != '\0';
      if (!is_variable)
	{
	  free (home_dir);
	  home_dir = getenv ("HOME");
	}
#if !defined(_AMIGA) && !defined(WIN32)
      if (home_dir == 0 || home_dir[0] == '\0')
	{
	  extern char *getlogin ();
	  char *name = getlogin ();
	  home_dir = 0;
	  if (name != 0)
	    {
	      struct passwd *p = getpwnam (name);
	      if (p != 0)
		home_dir = p->pw_dir;
	    }
	}
#endif /* !AMIGA && !WIN32 */
      if (home_dir != 0)
	{
	  char *new = concat (home_dir, "", name + 1);
	  if (is_variable)
	    free (home_dir);
	  return new;
	}
    }
#if !defined(_AMIGA) && !defined(WIN32)
  else
    {
      struct passwd *pwent;
      char *userend = index (name + 1, '/');
      if (userend != 0)
	*userend = '\0';
      pwent = getpwnam (name + 1);
      if (pwent != 0)
	{
	  if (userend == 0)
	    return savestring (pwent->pw_dir, strlen (pwent->pw_dir));
	  else
	    return concat (pwent->pw_dir, "/", userend + 1);
	}
      else if (userend != 0)
	*userend = '/';
    }
#endif /* !AMIGA && !WIN32 */
#endif /* !VMS */
  return 0;
}

/* Given a chain of struct nameseq's describing a sequence of filenames,
   in reverse of the intended order, return a new chain describing the
   result of globbing the filenames.  The new chain is in forward order.
   The links of the old chain are freed or used in the new chain.
   Likewise for the names in the old chain.

   SIZE is how big to construct chain elements.
   This is useful if we want them actually to be other structures
   that have room for additional info.  */

struct nameseq *
multi_glob (chain, size)
     struct nameseq *chain;
     unsigned int size;
{
  extern void dir_setup_glob ();
  register struct nameseq *new = 0;
  register struct nameseq *old;
  struct nameseq *nexto;
  glob_t gl;

  dir_setup_glob (&gl);

  for (old = chain; old != 0; old = nexto)
    {
#ifndef NO_ARCHIVES
      char *memname;
#endif

      nexto = old->next;

      if (old->name[0] == '~')
	{
	  char *newname = tilde_expand (old->name);
	  if (newname != 0)
	    {
	      free (old->name);
	      old->name = newname;
	    }
	}

#ifndef NO_ARCHIVES
      if (ar_name (old->name))
	{
	  /* OLD->name is an archive member reference.
	     Replace it with the archive file name,
	     and save the member name in MEMNAME.
	     We will glob on the archive name and then
	     reattach MEMNAME later.  */
	  char *arname;
	  ar_parse_name (old->name, &arname, &memname);
	  free (old->name);
	  old->name = arname;
	}
      else
	memname = 0;
#endif /* !NO_ARCHIVES */

      switch (glob (old->name, GLOB_NOCHECK|GLOB_ALTDIRFUNC, NULL, &gl))
	{
	case 0:			/* Success.  */
	  {
	    register int i = gl.gl_pathc;
	    while (i-- > 0)
	      {
#ifndef NO_ARCHIVES
		if (memname != 0)
		  {
		    /* Try to glob on MEMNAME within the archive.  */
		    struct nameseq *found
		      = ar_glob (gl.gl_pathv[i], memname, size);
		    if (found == 0)
		      {
			/* No matches.  Use MEMNAME as-is.  */
			struct nameseq *elt
			  = (struct nameseq *) xmalloc (size);
			unsigned int alen = strlen (gl.gl_pathv[i]);
			unsigned int mlen = strlen (memname);
			elt->name = (char *) xmalloc (alen + 1 + mlen + 2);
			bcopy (gl.gl_pathv[i], elt->name, alen);
			elt->name[alen] = '(';
			bcopy (memname, &elt->name[alen + 1], mlen);
			elt->name[alen + 1 + mlen] = ')';
			elt->name[alen + 1 + mlen + 1] = '\0';
			elt->next = new;
			new = elt;
		      }
		    else
		      {
			/* Find the end of the FOUND chain.  */
			struct nameseq *f = found;
			while (f->next != 0)
			  f = f->next;

			/* Attach the chain being built to the end of the FOUND
			   chain, and make FOUND the new NEW chain.  */
			f->next = new;
			new = found;
		      }

		    free (memname);
		  }
		else
#endif /* !NO_ARCHIVES */
		  {
		    struct nameseq *elt = (struct nameseq *) xmalloc (size);
		    elt->name = savestring (gl.gl_pathv[i],
					    strlen (gl.gl_pathv[i]));
		    elt->next = new;
		    new = elt;
		  }
	      }
	    globfree (&gl);
	    free (old->name);
	    free ((char *)old);
	    break;
	  }

	case GLOB_NOSPACE:
	  fatal ("virtual memory exhausted");
	  break;

	default:
	  old->next = new;
	  new = old;
	  break;
	}
    }

  return new;
}



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         remake.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Basic dependency engine for GNU Make.
Copyright (C) 1988,89,90,91,92,93,94,95,96 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
#include "job.h"
/* #include "commands.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "dep.h"  <- modification by J.Ruthruff, 7/20 */
#include <assert.h>
#undef stderr
#define stderr stdout

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#undef stderr
#define stderr stdout
#else
#include <sys/file.h>
#undef stderr
#define stderr stdout
#endif

#ifdef VMS
#include <starlet.h>
#undef stderr
#define stderr stdout
#endif
#ifdef WIN32
#include <io.h>
#undef stderr
#define stderr stdout
#endif

extern int try_implicit_rule PARAMS ((struct file *file, unsigned int depth));


/* Incremented when a command is started (under -n, when one would be).  */
unsigned int commands_started = 0;

static int update_file PARAMS ((struct file *file, unsigned int depth));
static int update_file_1 PARAMS ((struct file *file, unsigned int depth));
static int check_dep PARAMS ((struct file *file, unsigned int depth, time_t this_mtime, int *must_make_ptr));
static int touch_file PARAMS ((struct file *file));
static void remake_file PARAMS ((struct file *file));
static time_t name_mtime PARAMS ((char *name));
static int library_search PARAMS ((char **lib, time_t *mtime_ptr));

extern time_t f_mtime PARAMS ((struct file *file, int search));

#ifdef VMS
extern int vms_stat PARAMS ((char *name, struct stat *buf));
#endif


/* Remake all the goals in the `struct dep' chain GOALS.  Return -1 if nothing
   was done, 0 if all goals were updated successfully, or 1 if a goal failed.
   If MAKEFILES is nonzero, these goals are makefiles, so -t, -q, and -n should
   be disabled for them unless they were also command-line targets, and we
   should only make one goal at a time and return as soon as one goal whose
   `changed' member is nonzero is successfully made.  */

int
update_goal_chain (goals, makefiles)
     register struct dep *goals;
     int makefiles;
{
  int t = touch_flag, q = question_flag, n = just_print_flag;
  unsigned int j = job_slots;
  int status = -1;

#define	MTIME(file) (makefiles ? file_mtime_no_search (file) \
		     : file_mtime (file))

  /* Duplicate the chain so we can remove things from it.  */

  goals = copy_dep_chain (goals);

  {
    /* Clear the `changed' flag of each goal in the chain.
       We will use the flag below to notice when any commands
       have actually been run for a target.  When no commands
       have been run, we give an "up to date" diagnostic.  */

    struct dep *g;
    for (g = goals; g != 0; g = g->next)
      g->changed = 0;
  }

  if (makefiles)
    /* Only run one job at a time.  */
    job_slots = 1;

  /* Update all the goals until they are all finished.  */

  while (goals != 0)
    {
      register struct dep *g, *lastgoal;

      /* Start jobs that are waiting for the load to go down.  */

      start_waiting_jobs ();

      /* Wait for a child to die.  */

      reap_children (1, 0);

      lastgoal = 0;
      g = goals;
      while (g != 0)
	{
	  /* Iterate over all double-colon entries for this file.  */
	  struct file *file = g->file;
	  int stop, any_not_updated = 0;

	  for (file = g->file->double_colon ? g->file->double_colon : g->file;
	       file != NULL;
	       file = file->prev)
	    {
	      unsigned int ocommands_started;
	      int x;
	      time_t mtime = MTIME (file);
	      check_renamed (file);
	      if (makefiles)
		{
		  if (file->cmd_target)
		    {
		      touch_flag = t;
		      question_flag = q;
		      just_print_flag = n;
		    }
		  else
		    touch_flag = question_flag = just_print_flag = 0;
		}

	      /* Save the old value of `commands_started' so we can compare
		 later.  It will be incremented when any commands are
		 actually run.  */
	      ocommands_started = commands_started;

	      x = update_file (file, makefiles ? 1 : 0);
	      check_renamed (file);

	      /* Set the goal's `changed' flag if any commands were started
		 by calling update_file above.  We check this flag below to
		 decide when to give an "up to date" diagnostic.  */
	      g->changed += commands_started - ocommands_started;

	      stop = 0;
	      if (x != 0 || file->updated)
		{
		  /* If STATUS was not already 1, set it to 1 if
		     updating failed, or to 0 if updating succeeded.
		     Leave STATUS as it is if no updating was done.  */

		  if (status < 1)
		    {
		      if (file->update_status != 0)
			{
			  /* Updating failed, or -q triggered.
			     The STATUS value tells our caller which.  */
			  status = file->update_status;
			  /* If -q just triggered, stop immediately.
			     It doesn't matter how much more we run,
			     since we already know the answer to return.  */
			  stop = (!keep_going_flag && !question_flag
				  && !makefiles);
			}
		      else if (MTIME (file) != mtime)
			{
			  /* Updating was done.  If this is a makefile and
			     just_print_flag or question_flag is set
			     (meaning -n or -q was given and this file was
			     specified as a command-line target), don't
			     change STATUS.  If STATUS is changed, we will
			     get re-exec'd, and fall into an infinite loop.  */
			  if (!makefiles
			      || (!just_print_flag && !question_flag))
			    status = 0;
			  if (makefiles && file->dontcare)
			    /* This is a default makefile.  Stop remaking.  */
			    stop = 1;
			}
		    }
		}

	      /* Keep track if any double-colon entry is not finished.
                 When they are all finished, the goal is finished.  */
	      any_not_updated |= !file->updated;

	      if (stop)
		break;
	    }

	  /* Reset FILE since it is null at the end of the loop.  */
	  file = g->file;

	  if (stop || !any_not_updated)
	    {
	      /* If we have found nothing whatever to do for the goal,
		 print a message saying nothing needs doing.  */

	      if (!makefiles
		  /* If the update_status is zero, we updated successfully
		     or not at all.  G->changed will have been set above if
		     any commands were actually started for this goal.  */
		  && file->update_status == 0 && !g->changed
		  /* Never give a message under -s or -q.  */
		  && !silent_flag && !question_flag)
		message (1, ((file->phony || file->cmds == 0)
			     ? "Nothing to be done for `%s'."
			     : "`%s' is up to date."),
			 file->name);

	      /* This goal is finished.  Remove it from the chain.  */
	      if (lastgoal == 0)
		goals = g->next;
	      else
		lastgoal->next = g->next;

	      /* Free the storage.  */
	      free ((char *) g);

	      g = lastgoal == 0 ? goals : lastgoal->next;

	      if (stop)
		break;
	    }
	  else
	    {
	      lastgoal = g;
	      g = g->next;
	    }
	}
    }

  if (makefiles)
    {
      touch_flag = t;
      question_flag = q;
      just_print_flag = n;
      job_slots = j;
    }
  return status;
}

/* If FILE is not up to date, execute the commands for it.
   Return 0 if successful, 1 if unsuccessful;
   but with some flag settings, just call `exit' if unsuccessful.

   DEPTH is the depth in recursions of this function.
   We increment it during the consideration of our dependencies,
   then decrement it again after finding out whether this file
   is out of date.

   If there are multiple double-colon entries for FILE,
   each is considered in turn.  */

static int
update_file (file, depth)
     struct file *file;
     unsigned int depth;
{
  register int status = 0;
  register struct file *f;

  for (f = file->double_colon ? file->double_colon : file; f != 0; f = f->prev)
    {
      status |= update_file_1 (f, depth);
      check_renamed (f);

      if (status != 0 && !keep_going_flag)
	return status;

      switch (f->command_state)
	{
	case cs_finished:
	  /* The file is done being remade.  */
	  break;

	case cs_running: case_coverage = case_coverage;
	case cs_deps_running:
	  /* Don't run the other :: rules for this
	     file until this rule is finished.  */
	  return 0;

	default:
	  assert (f->command_state == cs_running);
	  break;
	}
    }

  return status;
}

/* Consider a single `struct file' and update it as appropriate.  */

static int
update_file_1 (file, depth)
     struct file *file;
     unsigned int depth;
{
  register time_t this_mtime;
  int noexist, must_make, deps_changed;
  int dep_status = 0;
  register struct dep *d, *lastd;
  int running = 0;

  DEBUGPR ("Considering target file `%s'.\n");

  if (file->updated)
    {
      if (file->update_status > 0)
	{
	  DEBUGPR ("Recently tried and failed to update file `%s'.\n");
	  return file->update_status;
	}

      DEBUGPR ("File `%s' was considered already.\n");
      return 0;
    }

  switch (file->command_state)
    {
    case cs_not_started: case_coverage = case_coverage;
    case cs_deps_running:
      break;
    case cs_running:
      DEBUGPR ("Still updating file `%s'.\n");
      return 0;
    case cs_finished:
      DEBUGPR ("Finished updating file `%s'.\n");
      return file->update_status;
    default:
      abort ();
    }

  ++depth;

  /* Notice recursive update of the same file.  */
  file->updating = 1;

  /* Looking at the file's modtime beforehand allows the possibility
     that its name may be changed by a VPATH search, and thus it may
     not need an implicit rule.  If this were not done, the file
     might get implicit commands that apply to its initial name, only
     to have that name replaced with another found by VPATH search.  */

  this_mtime = file_mtime (file);
  check_renamed (file);
  noexist = this_mtime == (time_t) -1;
  if (noexist)
    DEBUGPR ("File `%s' does not exist.\n");

  must_make = noexist;

  /* If file was specified as a target with no commands,
     come up with some default commands.  */

  if (!file->phony && file->cmds == 0 && !file->tried_implicit)
    {
      if (try_implicit_rule (file, depth))
	DEBUGPR ("Found an implicit rule for `%s'.\n");
      else
	DEBUGPR ("No implicit rule found for `%s'.\n");
      file->tried_implicit = 1;
    }
  if (file->cmds == 0 && !file->is_target
      && default_file != 0 && default_file->cmds != 0)
    {
      DEBUGPR ("Using default commands for `%s'.\n");
      file->cmds = default_file->cmds;
    }

  /* Update all non-intermediate files we depend on, if necessary,
     and see whether any of them is more recent than this file.  */

  lastd = 0;
  d = file->deps;
  while (d != 0)
    {
      time_t mtime;

      check_renamed (d->file);

      mtime = file_mtime (d->file);
      check_renamed (d->file);

      if (d->file->updating)
	{
	  error ("Circular %s <- %s dependency dropped.",
		 file->name, d->file->name);
	  if (lastd == 0)
	    {
	      file->deps = d->next;
	      free ((char *) d);
	      d = file->deps;
	    }
	  else
	    {
	      lastd->next = d->next;
	      free ((char *) d);
	      d = lastd->next;
	    }
	  continue;
	}

      d->file->parent = file;
      dep_status |= check_dep (d->file, depth, this_mtime, &must_make);
      check_renamed (d->file);

      {
	register struct file *f = d->file;
	if (f->double_colon)
	  f = f->double_colon;
	do
	  {
	    running |= (f->command_state == cs_running
			|| f->command_state == cs_deps_running);
	    f = f->prev;
	  }
	while (f != 0);
      }

      if (dep_status != 0 && !keep_going_flag)
	break;

      if (!running)
	d->changed = file_mtime (d->file) != mtime;

      lastd = d;
      d = d->next;
    }

  /* Now we know whether this target needs updating.
     If it does, update all the intermediate files we depend on.  */

  if (must_make)
    {
      for (d = file->deps; d != 0; d = d->next)
	if (d->file->intermediate)
	  {
	    time_t mtime = file_mtime (d->file);
	    check_renamed (d->file);
	    d->file->parent = file;
	    dep_status |= update_file (d->file, depth);
	    check_renamed (d->file);

	    {
	      register struct file *f = d->file;
	      if (f->double_colon)
		f = f->double_colon;
	      do
		{
		  running |= (f->command_state == cs_running
			      || f->command_state == cs_deps_running);
		  f = f->prev;
		}
	      while (f != 0);
	    }

	    if (dep_status != 0 && !keep_going_flag)
	      break;

	    if (!running)
	      d->changed = ((file->phony && file->cmds != 0)
			    || file_mtime (d->file) != mtime);
	  }
    }

  file->updating = 0;

  DEBUGPR ("Finished dependencies of target file `%s'.\n");

  if (running)
    {
      set_command_state (file, cs_deps_running);
      --depth;
      DEBUGPR ("The dependencies of `%s' are being made.\n");
      return 0;
    }

  /* If any dependency failed, give up now.  */

  if (dep_status != 0)
    {
      file->update_status = dep_status;
      notice_finished_file (file);

      depth--;

      DEBUGPR ("Giving up on target file `%s'.\n");

      if (depth == 0 && keep_going_flag
	  && !just_print_flag && !question_flag)
	error ("Target `%s' not remade because of errors.", file->name);

      return dep_status;
    }

  if (file->command_state == cs_deps_running)
    /* The commands for some deps were running on the last iteration, but
       they have finished now.  Reset the command_state to not_started to
       simplify later bookkeeping.  It is important that we do this only
       when the prior state was cs_deps_running, because that prior state
       was definitely propagated to FILE's also_make's by set_command_state
       (called above), but in another state an also_make may have
       independently changed to finished state, and we would confuse that
       file's bookkeeping (updated, but not_started is bogus state).  */
    set_command_state (file, cs_not_started);

  /* Now record which dependencies are more
     recent than this file, so we can define $?.  */

  deps_changed = 0;
  for (d = file->deps; d != 0; d = d->next)
    {
      time_t d_mtime = file_mtime (d->file);
      check_renamed (d->file);

#if 1	/* %%% In version 4, remove this code completely to
	   implement not remaking deps if their deps are newer
	   than their parents.  */
      if (d_mtime == (time_t) -1 && !d->file->intermediate)
	/* We must remake if this dep does not
	   exist and is not intermediate.  */
	must_make = 1;
#endif

      /* Set DEPS_CHANGED if this dep actually changed.  */
      deps_changed |= d->changed;

      /* Set D->changed if either this dep actually changed,
	 or its dependent, FILE, is older or does not exist.  */
      d->changed |= noexist || d_mtime > this_mtime;

      if (debug_flag && !noexist)
	{
	  print_spaces (depth);
	  if (d_mtime == (time_t) -1)
	    printf ("Dependency `%s' does not exist.\n", dep_name (d));
	  else
	    printf ("Dependency `%s' is %s than dependent `%s'.\n",
		    dep_name (d), d->changed ? "newer" : "older", file->name);
	  fflush (stdout);
	}
    }

  /* Here depth returns to the value it had when we were called.  */
  depth--;

  if (file->double_colon && file->deps == 0)
    {
      must_make = 1;
      DEBUGPR ("Target `%s' is double-colon and has no dependencies.\n");
    }
  else if (!noexist && file->is_target && !deps_changed && file->cmds == 0)
    {
      must_make = 0;
      DEBUGPR ("No commands for `%s' and no dependencies actually changed.\n");
    }

  if (!must_make)
    {
      DEBUGPR ("No need to remake target `%s'.\n");
      notice_finished_file (file);
      return 0;
    }

  DEBUGPR ("Must remake target `%s'.\n");

  /* Now, take appropriate actions to remake the file.  */
  remake_file (file);

  if (file->command_state != cs_finished)
    {
      DEBUGPR ("Commands of `%s' are being run.\n");
      return 0;
    }

  switch (file->update_status)
    {
    case 2:
      DEBUGPR ("Failed to remake target file `%s'.\n");
      break;
    case 0:
      DEBUGPR ("Successfully remade target file `%s'.\n");
      break;
    case 1:
      DEBUGPR ("Target file `%s' needs remade under -q.\n");
      break;
    default:
      assert (file->update_status >= 0 && file->update_status <= 2);
      break;
    }

  file->updated = 1;
  return file->update_status;
}

/* Set FILE's `updated' flag and re-check its mtime and the mtime's of all
   files listed in its `also_make' member.  Under -t, this function also
   touches FILE.

   On return, FILE->update_status will no longer be -1 if it was.  */

void
notice_finished_file (file)
     register struct file *file;
{
  struct dep *d;
  int ran = file->command_state == cs_running;

  file->command_state = cs_finished;
  file->updated = 1;

  if (touch_flag
      /* The update status will be:
	 	-1	if this target was not remade;
		0	if 0 or more commands (+ or ${MAKE}) were run and won;
		1	if some commands were run and lost.
	 We touch the target if it has commands which either were not run
	 or won when they ran (i.e. status is 0).  */
      && file->update_status == 0)
    {
      if (file->cmds != 0 && file->cmds->any_recurse)
	{
	  /* If all the command lines were recursive,
	     we don't want to do the touching.  */
	  unsigned int i;
	  for (i = 0; i < file->cmds->ncommand_lines; ++i)
	    if (!(file->cmds->lines_flags[i] & COMMANDS_RECURSE))
	      goto have_nonrecursing;
	}
      else
	{
	have_nonrecursing:
	  if (file->phony)
	    file->update_status = 0;
	  else
	    /* Should set file's modification date and do nothing else.  */
	    file->update_status = touch_file (file);
	}
    }

  if (ran && !file->phony)
    {
      struct file *f;

      if (just_print_flag || question_flag
	  || (file->is_target && file->cmds == 0))
	file->last_mtime = NEW_MTIME;
      else
	file->last_mtime = 0;

      /* Propagate the change of modification time to all the double-colon
	 entries for this file.  */
      for (f = file->double_colon; f != 0; f = f->next)
	f->last_mtime = file->last_mtime;
    }

  if (ran && file->update_status != -1)
    /* We actually tried to update FILE, which has
       updated its also_make's as well (if it worked).
       If it didn't work, it wouldn't work again for them.
       So mark them as updated with the same status.  */
    for (d = file->also_make; d != 0; d = d->next)
      {
	d->file->command_state = cs_finished;
	d->file->updated = 1;
	d->file->update_status = file->update_status;

	if (ran && !d->file->phony)
	  /* Fetch the new modification time.
	     We do this instead of just invalidating the cached time
	     so that a vpath_search can happen.  Otherwise, it would
	     never be done because the target is already updated.  */
	  (void) f_mtime (d->file, 0);
      }
  else if (file->update_status == -1)
    /* Nothing was done for FILE, but it needed nothing done.
       So mark it now as "succeeded".  */
    file->update_status = 0;
}

/* Check whether another file (whose mtime is THIS_MTIME)
   needs updating on account of a dependency which is file FILE.
   If it does, store 1 in *MUST_MAKE_PTR.
   In the process, update any non-intermediate files
   that FILE depends on (including FILE itself).
   Return nonzero if any updating failed.  */

static int
check_dep (file, depth, this_mtime, must_make_ptr)
     struct file *file;
     unsigned int depth;
     time_t this_mtime;
     int *must_make_ptr;
{
  register struct dep *d;
  int dep_status = 0;

  ++depth;
  file->updating = 1;

  if (!file->intermediate)
    /* If this is a non-intermediate file, update it and record
       whether it is newer than THIS_MTIME.  */
    {
      time_t mtime;
      dep_status = update_file (file, depth);
      check_renamed (file);
      mtime = file_mtime (file);
      check_renamed (file);
      if (mtime == (time_t) -1 || mtime > this_mtime)
	*must_make_ptr = 1;
    }
  else
    {
      /* FILE is an intermediate file.  */
      time_t mtime;

      if (!file->phony && file->cmds == 0 && !file->tried_implicit
	  && file->secondary)
	{
	  if (try_implicit_rule (file, depth))
	    DEBUGPR ("Found an implicit rule for `%s'.\n");
	  else
	    DEBUGPR ("No implicit rule found for `%s'.\n");
	  file->tried_implicit = 1;
	}
      if (file->cmds == 0 && !file->is_target && file->secondary
	  && default_file != 0 && default_file->cmds != 0)
	{
	  DEBUGPR ("Using default commands for `%s'.\n");
	  file->cmds = default_file->cmds;
	}

      /* If the intermediate file actually exists
	 and is newer, then we should remake from it.  */
      check_renamed (file);
      mtime = file_mtime (file);
      check_renamed (file);
      if (mtime > this_mtime)
	*must_make_ptr = 1;
	  /* Otherwise, update all non-intermediate files we depend on,
	     if necessary, and see whether any of them is more
	     recent than the file on whose behalf we are checking.  */
      else
	{
	  register struct dep *lastd;

	  lastd = 0;
	  d = file->deps;
	  while (d != 0)
	    {
	      if (d->file->updating)
		{
		  error ("Circular %s <- %s dependency dropped.",
			 file->name, d->file->name);
		  if (lastd == 0)
		    {
		      file->deps = d->next;
		      free ((char *) d);
		      d = file->deps;
		    }
		  else
		    {
		      lastd->next = d->next;
		      free ((char *) d);
		      d = lastd->next;
		    }
		  continue;
		}

	      d->file->parent = file;
	      dep_status |= check_dep (d->file, depth, this_mtime, must_make_ptr);
	      check_renamed (d->file);
	      if (dep_status != 0 && !keep_going_flag)
		break;

	      if (d->file->command_state == cs_running
		  || d->file->command_state == cs_deps_running)
		/* Record that some of FILE's dependencies are still being made.
		   This tells the upper levels to wait on processing it until
		   the commands are finished.  */
		set_command_state (file, cs_deps_running);

	      lastd = d;
	      d = d->next;
	    }
	}
    }

  file->updating = 0;
  return dep_status;
}

/* Touch FILE.  Return zero if successful, one if not.  */

#define TOUCH_ERROR(call) return (perror_with_name (call, file->name), 1)

static int
touch_file (file)
     register struct file *file;
{
  if (!silent_flag)
    message (0, "touch %s", file->name);

#ifndef	NO_ARCHIVES
  if (ar_name (file->name))
    return ar_touch (file->name);
  else
#endif
    {
      int fd = open (file->name, O_RDWR | O_CREAT, 0666);

      if (fd < 0)
	TOUCH_ERROR ("touch: open: ");
      else
	{
	  struct stat statbuf;
	  char buf;
	  int status;

#ifdef EINTR
	  do
#endif
	    status = fstat (fd, &statbuf);
#ifdef EINTR
	  while (status < 0 && errno == EINTR);
#endif
	  if (status < 0)
	    TOUCH_ERROR ("touch: fstat: ");
	  /* Rewrite character 0 same as it already is.  */
	  if (read (fd, &buf, 1) < 0)
	    TOUCH_ERROR ("touch: read: ");
	  if (lseek (fd, 0L, 0) < 0L)
	    TOUCH_ERROR ("touch: lseek: ");
	  if (write (fd, &buf, 1) < 0)
	    TOUCH_ERROR ("touch: write: ");
	  /* If file length was 0, we just
	     changed it, so change it back.  */
	  if (statbuf.st_size == 0)
	    {
	      (void) close (fd);
	      fd = open (file->name, O_RDWR | O_TRUNC, 0666);
	      if (fd < 0)
		TOUCH_ERROR ("touch: open: ");
	    }
	  (void) close (fd);
	}
    }

  return 0;
}

/* Having checked and updated the dependencies of FILE,
   do whatever is appropriate to remake FILE itself.
   Return the status from executing FILE's commands.  */

static void
remake_file (file)
     struct file *file;
{
  if (file->cmds == 0)
    {
      if (file->phony)
	/* Phony target.  Pretend it succeeded.  */
	file->update_status = 0;
      else if (file->is_target)
	/* This is a nonexistent target file we cannot make.
	   Pretend it was successfully remade.  */
	file->update_status = 0;
      else
	{
	  /* This is a dependency file we cannot remake.  Fail.  */
	  static const char msg_noparent[]
	    = "%sNo rule to make target `%s'%s";
	  static const char msg_parent[]
	    = "%sNo rule to make target `%s', needed by `%s'%s";
	  if (keep_going_flag || file->dontcare)
	    {
	      if (!file->dontcare)
		{
		  if (file->parent == 0)
		    error (msg_noparent, "*** ", file->name, ".");
		  else
		    error (msg_parent, "*** ",
			   file->name, file->parent->name, ".");
		}
 	      file->update_status = 2;
	    }
	  else
	    {
	      if (file->parent == 0)
		fatal (msg_noparent, "", file->name, "");
	      else
		fatal (msg_parent, "", file->name, file->parent->name, "");
	    }
	}
    }
  else
    {
      chop_commands (file->cmds);

      if (!touch_flag || file->cmds->any_recurse)
	{
	  execute_file_commands (file);
	  return;
	}
      else
	/* This tells notice_finished_file it is ok to touch the file.  */
	file->update_status = 0;
    }

  /* This does the touching under -t.  */
  notice_finished_file (file);
}

/* Return the mtime of a file, given a `struct file'.
   Caches the time in the struct file to avoid excess stat calls.

   If the file is not found, and SEARCH is nonzero, VPATH searching and
   replacement is done.  If that fails, a library (-lLIBNAME) is tried and
   the library's actual name (/lib/libLIBNAME.a, etc.) is substituted into
   FILE.  */

time_t
f_mtime (file, search)
     register struct file *file;
     int search;
{
  time_t mtime;

  /* File's mtime is not known; must get it from the system.  */

#ifndef	NO_ARCHIVES
  if (ar_name (file->name))
    {
      /* This file is an archive-member reference.  */

      char *arname, *memname;
      struct file *arfile;
      int arname_used = 0;

      /* Find the archive's name.  */
      ar_parse_name (file->name, &arname, &memname);

      /* Find the modification time of the archive itself.
	 Also allow for its name to be changed via VPATH search.  */
      arfile = lookup_file (arname);
      if (arfile == 0)
	{
	  arfile = enter_file (arname);
	  arname_used = 1;
	}
      mtime = f_mtime (arfile, search);
      check_renamed (arfile);
      if (search && strcmp (arfile->name, arname))
	{
	  /* The archive's name has changed.
	     Change the archive-member reference accordingly.  */

	  unsigned int arlen, memlen;

	  if (!arname_used)
	    {
	      free (arname);
	      arname_used = 1;
	    }

	  arname = arfile->name;
	  arlen = strlen (arname);
	  memlen = strlen (memname);

	  free (file->name);

	  file->name = (char *) xmalloc (arlen + 1 + memlen + 2);
	  bcopy (arname, file->name, arlen);
	  file->name[arlen] = '(';
	  bcopy (memname, file->name + arlen + 1, memlen);
	  file->name[arlen + 1 + memlen] = ')';
	  file->name[arlen + 1 + memlen + 1] = '\0';
	}

      if (!arname_used)
	free (arname);
      free (memname);

      if (mtime == (time_t) -1)
	/* The archive doesn't exist, so it's members don't exist either.  */
	return (time_t) -1;

      mtime = ar_member_date (file->name);
    }
  else
#endif
    {
      mtime = name_mtime (file->name);

      if (mtime == (time_t) -1 && search)
	{
	  /* If name_mtime failed, search VPATH.  */
	  char *name = file->name;
	  if (vpath_search (&name, &mtime)
	      /* Last resort, is it a library (-lxxx)?  */
	      || (name[0] == '-' && name[1] == 'l'
		  && library_search (&name, &mtime)))
	    {
	      if (mtime != 0)
		/* vpath_search and library_search store zero in MTIME
		   if they didn't need to do a stat call for their work.  */
		file->last_mtime = mtime;
	      rename_file (file, name);
	      check_renamed (file);
	      return file_mtime (file);
	    }
	}
    }

  {
    /* Files can have bogus timestamps that nothing newly made will be
       "newer" than.  Updating their dependents could just result in loops.
       So notify the user of the anomaly by treating future files as
       unrecoverably absent.  */

    static time_t now;
    if (mtime != -1 && mtime > now && ! file->updated)
      {
	/* This file's time appears to be in the future.
	   Update our concept of the present, and compare again.  */
#ifdef VMS
	/* Handle vms 64bit to 32bit time hack introduced in vms_stat ... */
	static unsigned long vms_now[2]; /* assumes 32 bit long ! */
	sys$gettim (vms_now);
	now = ((vms_now[0]>>24) & 0xff) + ((vms_now[1]<<8) & 0xffffff00);
	if (mtime > now)
#else
	extern time_t time ();
	if (mtime > time (&now))
#endif
	  {
	    error ("*** File `%s' has modification time in the future",
		   file->name);
	    file->update_status = 2;
	  }
      }
  }

  /* Store the mtime into all the entries for this file.  */
  if (file->double_colon)
    file = file->double_colon;
  do
    {
      file->last_mtime = mtime;
      file = file->prev;
    } while (file != 0);

  return mtime;
}


/* Return the mtime of the file or archive-member reference NAME.  */

static time_t
name_mtime (name)
     register char *name;
{
  struct stat st;

#ifdef VMS
  if (vms_stat (name, &st) < 0)
#else
  if (stat (name, &st) < 0)
#endif
    return (time_t) -1;

  return (time_t) st.st_mtime;
}


/* Search for a library file specified as -lLIBNAME, searching for a
   suitable library file in the system library directories and the VPATH
   directories.  */

static int
library_search (lib, mtime_ptr)
     char **lib;
     time_t *mtime_ptr;
{
  static char *dirs[] =
    {
#ifndef _AMIGA
      "/lib",
      "/usr/lib",
#endif
#if defined(WIN32) && !defined(LIBDIR)
/*
 * This is completely up to the user at product install time. Just define
 * a placeholder.
 */
#define LIBDIR "/usr/local/lib"  /* modification by J.Ruthruff, 8/16 */
#endif
      LIBDIR,			/* Defined by configuration.  */
      0
    };

  char *libname = &(*lib)[2];	/* Name without the `-l'.  */
  time_t mtime;

  /* Buffer to construct possible names in.  */
  char *buf = xmalloc (sizeof (LIBDIR) + 8 + strlen (libname) + 4 + 2 + 1);
  char *file, **dp;

  /* Look first for `libNAME.a' in the current directory.  */

#ifndef _AMIGA
  sprintf (buf, "lib%s.a", libname);
#else
  sprintf (buf, "%s.lib", libname);
#endif
  mtime = name_mtime (buf);
  if (mtime != (time_t) -1)
    {
      *lib = buf;
      if (mtime_ptr != 0)
	*mtime_ptr = mtime;
      return 1;
    }

  /* Now try VPATH search on that.  */

  file = buf;
  if (vpath_search (&file, mtime_ptr))
    {
      free (buf);
      *lib = file;
      return 1;
    }

  /* Now try the standard set of directories.  */

  for (dp = dirs; *dp != 0; ++dp)
    {
#ifndef _AMIGA
      sprintf (buf, "%s/lib%s.a", *dp, libname);
#else
      sprintf (buf, "%s/%s.lib", *dp, libname);
#endif
      mtime = name_mtime (buf);
      if (mtime != (time_t) -1)
	{
	  *lib = buf;
	  if (mtime_ptr != 0)
	    *mtime_ptr = mtime;
	  return 1;
	}
    }

  free (buf);
  return 0;
}



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         rule.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Pattern and suffix rule internals for GNU Make.
Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "dep.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
#include "job.h"
/* #include "commands.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "variable.h"  <- modification by J.Ruthruff, 7/20 */
#include "rule.h"
#undef stderr
#define stderr stdout

static void freerule PARAMS ((struct rule *rule, struct rule *lastrule));

/* Chain of all pattern rules.  */

struct rule *pattern_rules;

/* Pointer to last rule in the chain, so we can add onto the end.  */

struct rule *last_pattern_rule;

/* Number of rules in the chain.  */

unsigned int num_pattern_rules;

/* Maximum number of target patterns of any pattern rule.  */

unsigned int max_pattern_targets;

/* Maximum number of dependencies of any pattern rule.  */

unsigned int max_pattern_deps;

/* Maximum length of the name of a dependencies of any pattern rule.  */

unsigned int max_pattern_dep_length;

/* Pointer to structure for the file .SUFFIXES
   whose dependencies are the suffixes to be searched.  */

struct file *suffix_file;

/* Maximum length of a suffix.  */

unsigned int maxsuffix;

/* Compute the maximum dependency length and maximum number of
   dependencies of all implicit rules.  Also sets the subdir
   flag for a rule when appropriate, possibly removing the rule
   completely when appropriate.  */

void
count_implicit_rule_limits ()
{
  char *name;
  unsigned int namelen;
  register struct rule *rule, *lastrule;

  num_pattern_rules = max_pattern_targets = max_pattern_deps = 0;
  max_pattern_dep_length = 0;

  name = 0;
  namelen = 0;
  rule = pattern_rules;
  lastrule = 0;
  while (rule != 0)
    {
      unsigned int ndeps = 0;
      register struct dep *dep;
      struct rule *next = rule->next;
      unsigned int ntargets;

      ++num_pattern_rules;

      ntargets = 0;
      while (rule->targets[ntargets] != 0)
	++ntargets;

      if (ntargets > max_pattern_targets)
	max_pattern_targets = ntargets;

      for (dep = rule->deps; dep != 0; dep = dep->next)
	{
	  unsigned int len = strlen (dep->name);

#ifdef VMS
	  char *p = rindex (dep->name, ']');
#else
	  char *p = rindex (dep->name, '/');
#endif
	  char *p2 = p != 0 ? index (dep->name, '%') : 0;
	  ndeps++;

	  if (len > max_pattern_dep_length)
	    max_pattern_dep_length = len;

	  if (p != 0 && p2 > p)
	    {
	      /* There is a slash before the % in the dep name.
		 Extract the directory name.  */
	      if (p == dep->name)
		++p;
	      if (p - dep->name > namelen)
		{
		  if (name != 0)
		    free (name);
		  namelen = p - dep->name;
		  name = (char *) xmalloc (namelen + 1);
		}
	      bcopy (dep->name, name, p - dep->name);
	      name[p - dep->name] = '\0';

	      /* In the deps of an implicit rule the `changed' flag
		 actually indicates that the dependency is in a
		 nonexistent subdirectory.  */

	      dep->changed = !dir_file_exists_p (name, "");
#ifdef VMS
	      if (dep->changed && *name == ']')
#else
	      if (dep->changed && *name == '/')
#endif
		{
		  /* The name is absolute and the directory does not exist.
		     This rule can never possibly match, since this dependency
		     can never possibly exist.  So just remove the rule from
		     the list.  */
		  freerule (rule, lastrule);
		  --num_pattern_rules;
		  goto end_main_loop;
		}
	    }
	  else
	    /* This dependency does not reside in a subdirectory.  */
	    dep->changed = 0;
	}

      if (ndeps > max_pattern_deps)
	max_pattern_deps = ndeps;

      lastrule = rule;
    end_main_loop:
      rule = next;
    }

  if (name != 0)
    free (name);
}

/* Create a pattern rule from a suffix rule.
   TARGET is the target suffix; SOURCE is the source suffix.
   CMDS are the commands.
   If TARGET is nil, it means the target pattern should be `(%.o)'.
   If SOURCE is nil, it means there should be no deps.  */

static void
convert_suffix_rule (target, source, cmds)
     char *target, *source;
     struct commands *cmds;
{
  char *targname, *targpercent, *depname;
  char **names, **percents;
  struct dep *deps;
  unsigned int len;

  if (target == 0)
    /* Special case: TARGET being nil means we are defining a
       `.X.a' suffix rule; the target pattern is always `(%.o)'.  */
    {
#ifdef VMS
      targname = savestring ("(%.obj)", 7);
#else
      targname = savestring ("(%.o)", 5);
#endif
      targpercent = targname + 1;
    }
  else
    {
      /* Construct the target name.  */
      len = strlen (target);
      targname = xmalloc (1 + len + 1);
      targname[0] = '%';
      bcopy (target, targname + 1, len + 1);
      targpercent = targname;
    }

  names = (char **) xmalloc (2 * sizeof (char *));
  percents = (char **) alloca (2 * sizeof (char *));
  names[0] = targname;
  percents[0] = targpercent;
  names[1] = percents[1] = 0;

  if (source == 0)
    deps = 0;
  else
    {
      /* Construct the dependency name.  */
      len = strlen (source);
      depname = xmalloc (1 + len + 1);
      depname[0] = '%';
      bcopy (source, depname + 1, len + 1);
      deps = (struct dep *) xmalloc (sizeof (struct dep));
      deps->next = 0;
      deps->name = depname;
    }

  create_pattern_rule (names, percents, 0, deps, cmds, 0);
}

/* Convert old-style suffix rules to pattern rules.
   All rules for the suffixes on the .SUFFIXES list
   are converted and added to the chain of pattern rules.  */

void
convert_to_pattern ()
{
  register struct dep *d, *d2;
  register struct file *f;
  register char *rulename;
  register unsigned int slen, s2len;

  /* Compute maximum length of all the suffixes.  */

  maxsuffix = 0;
  for (d = suffix_file->deps; d != 0; d = d->next)
    {
      register unsigned int namelen = strlen (dep_name (d));
      if (namelen > maxsuffix)
	maxsuffix = namelen;
    }

  rulename = (char *) alloca ((maxsuffix * 2) + 1);

  for (d = suffix_file->deps; d != 0; d = d->next)
    {
      /* Make a rule that is just the suffix, with no deps or commands.
	 This rule exists solely to disqualify match-anything rules.  */
      convert_suffix_rule (dep_name (d), (char *) 0, (struct commands *) 0);

      f = d->file;
      if (f->cmds != 0)
	/* Record a pattern for this suffix's null-suffix rule.  */
	convert_suffix_rule ("", dep_name (d), f->cmds);

      /* Record a pattern for each of this suffix's two-suffix rules.  */
      slen = strlen (dep_name (d));
      bcopy (dep_name (d), rulename, slen);
      for (d2 = suffix_file->deps; d2 != 0; d2 = d2->next)
	{
	  s2len = strlen (dep_name (d2));

	  if (slen == s2len && streq (dep_name (d), dep_name (d2)))
	    continue;

	  bcopy (dep_name (d2), rulename + slen, s2len + 1);
	  f = lookup_file (rulename);
	  if (f == 0 || f->cmds == 0)
	    continue;

	  if (s2len == 2 && rulename[slen] == '.' && rulename[slen + 1] == 'a')
	    /* A suffix rule `.X.a:' generates the pattern rule `(%.o): %.X'.
	       It also generates a normal `%.a: %.X' rule below.  */
	    convert_suffix_rule ((char *) 0, /* Indicates `(%.o)'.  */
				 dep_name (d),
				 f->cmds);

	  /* The suffix rule `.X.Y:' is converted
	     to the pattern rule `%.Y: %.X'.  */
	  convert_suffix_rule (dep_name (d2), dep_name (d), f->cmds);
	}
    }
}


/* Install the pattern rule RULE (whose fields have been filled in)
   at the end of the list (so that any rules previously defined
   will take precedence).  If this rule duplicates a previous one
   (identical target and dependencies), the old one is replaced
   if OVERRIDE is nonzero, otherwise this new one is thrown out.
   When an old rule is replaced, the new one is put at the end of the
   list.  Return nonzero if RULE is used; zero if not.  */

int
new_pattern_rule (rule, override)
     register struct rule *rule;
     int override;
{
  register struct rule *r, *lastrule;
  register unsigned int i, j;

  rule->in_use = 0;
  rule->terminal = 0;

  rule->next = 0;

  /* Search for an identical rule.  */
  lastrule = 0;
  for (r = pattern_rules; r != 0; lastrule = r, r = r->next)
    for (i = 0; rule->targets[i] != 0; ++i)
      {
	for (j = 0; r->targets[j] != 0; ++j)
	  if (!streq (rule->targets[i], r->targets[j]))
	    break;
	if (r->targets[j] == 0)
	  /* All the targets matched.  */
	  {
	    register struct dep *d, *d2;
	    for (d = rule->deps, d2 = r->deps;
		 d != 0 && d2 != 0; d = d->next, d2 = d2->next)
	      if (!streq (dep_name (d), dep_name (d2)))
		break;
	    if (d == 0 && d2 == 0)
	      /* All the dependencies matched.  */
	      if (override)
		{
		  /* Remove the old rule.  */
		  freerule (r, lastrule);
		  /* Install the new one.  */
		  if (pattern_rules == 0)
		    pattern_rules = rule;
		  else
		    last_pattern_rule->next = rule;
		  last_pattern_rule = rule;

		  /* We got one.  Stop looking.  */
		  goto matched;
		}
	      else
		{
		  /* The old rule stays intact.  Destroy the new one.  */
		  freerule (rule, (struct rule *) 0);
		  return 0;
		}
	  }
      }

 matched:;

  if (r == 0)
    {
      /* There was no rule to replace.  */
      if (pattern_rules == 0)
	pattern_rules = rule;
      else
	last_pattern_rule->next = rule;
      last_pattern_rule = rule;
    }

  return 1;
}


/* Install an implicit pattern rule based on the three text strings
   in the structure P points to.  These strings come from one of
   the arrays of default implicit pattern rules.
   TERMINAL specifies what the `terminal' field of the rule should be.  */

void
install_pattern_rule (p, terminal)
     struct pspec *p;
     int terminal;
{
  register struct rule *r;
  char *ptr;

  r = (struct rule *) xmalloc (sizeof (struct rule));

  r->targets = (char **) xmalloc (2 * sizeof (char *));
  r->suffixes = (char **) xmalloc (2 * sizeof (char *));
  r->lens = (unsigned int *) xmalloc (2 * sizeof (unsigned int));

  r->targets[1] = 0;
  r->suffixes[1] = 0;
  r->lens[1] = 0;

  r->lens[0] = strlen (p->target);
  /* These will all be string literals, but we malloc space for
     them anyway because somebody might want to free them later on.  */
  r->targets[0] = savestring (p->target, r->lens[0]);
  r->suffixes[0] = find_percent (r->targets[0]);
  if (r->suffixes[0] == 0)
    /* Programmer-out-to-lunch error.  */
    abort ();
  else
    ++r->suffixes[0];

  ptr = p->dep;
  r->deps = (struct dep *) multi_glob (parse_file_seq (&ptr, '\0',
                                                       sizeof (struct dep), 1),
				       sizeof (struct dep));

  if (new_pattern_rule (r, 0))
    {
      r->terminal = terminal;
      r->cmds = (struct commands *) xmalloc (sizeof (struct commands));
      r->cmds->filename = 0;
      r->cmds->lineno = 0;
      /* These will all be string literals, but we malloc space for them
	 anyway because somebody might want to free them later.  */
      r->cmds->commands = savestring (p->commands, strlen (p->commands));
      r->cmds->command_lines = 0;
    }
}


/* Free all the storage used in RULE and take it out of the
   pattern_rules chain.  LASTRULE is the rule whose next pointer
   points to RULE.  */

static void
freerule (rule, lastrule)
     register struct rule *rule, *lastrule;
{
  struct rule *next = rule->next;
  register unsigned int i;

  for (i = 0; rule->targets[i] != 0; ++i)
    free (rule->targets[i]);

  free ((char *) rule->targets);
  free ((char *) rule->suffixes);
  free ((char *) rule->lens);

  /* We can't free the storage for the commands because there
     are ways that they could be in more than one place:
       * If the commands came from a suffix rule, they could also be in
       the `struct file's for other suffix rules or plain targets given
       on the same makefile line.
       * If two suffixes that together make a two-suffix rule were each
       given twice in the .SUFFIXES list, and in the proper order, two
       identical pattern rules would be created and the second one would
       be discarded here, but both would contain the same `struct commands'
       pointer from the `struct file' for the suffix rule.  */

  free ((char *) rule);

  if (pattern_rules == rule)
    if (lastrule != 0)
      abort ();
    else
      pattern_rules = next;
  else if (lastrule != 0)
    lastrule->next = next;
  if (last_pattern_rule == rule)
    last_pattern_rule = lastrule;
}

/* Create a new pattern rule with the targets in the nil-terminated
   array TARGETS.  If TARGET_PERCENTS is not nil, it is an array of
   pointers into the elements of TARGETS, where the `%'s are.
   The new rule has dependencies DEPS and commands from COMMANDS.
   It is a terminal rule if TERMINAL is nonzero.  This rule overrides
   identical rules with different commands if OVERRIDE is nonzero.

   The storage for TARGETS and its elements is used and must not be freed
   until the rule is destroyed.  The storage for TARGET_PERCENTS is not used;
   it may be freed.  */

void
create_pattern_rule (targets, target_percents,
		     terminal, deps, commands, override)
     char **targets, **target_percents;
     int terminal;
     struct dep *deps;
     struct commands *commands;
     int override;
{
  register struct rule *r = (struct rule *) xmalloc (sizeof (struct rule));
  register unsigned int max_targets, i;

  r->cmds = commands;
  r->deps = deps;
  r->targets = targets;

  max_targets = 2;
  r->lens = (unsigned int *) xmalloc (2 * sizeof (unsigned int));
  r->suffixes = (char **) xmalloc (2 * sizeof (char *));
  for (i = 0; targets[i] != 0; ++i)
    {
      if (i == max_targets - 1)
	{
	  max_targets += 5;
	  r->lens = (unsigned int *)
	    xrealloc ((char *) r->lens, max_targets * sizeof (unsigned int));
	  r->suffixes = (char **)
	    xrealloc ((char *) r->suffixes, max_targets * sizeof (char *));
	}
      r->lens[i] = strlen (targets[i]);
      r->suffixes[i] = (target_percents == 0 ? find_percent (targets[i])
			: target_percents[i]) + 1;
      if (r->suffixes[i] == 0)
	abort ();
    }

  if (i < max_targets - 1)
    {
      r->lens = (unsigned int *) xrealloc ((char *) r->lens,
					   (i + 1) * sizeof (unsigned int));
      r->suffixes = (char **) xrealloc ((char *) r->suffixes,
					(i + 1) * sizeof (char *));
    }

  if (new_pattern_rule (r, override))
    r->terminal = terminal;
}

/* Print the data base of rules.  */

static void			/* Useful to call from gdb.  */
print_rule (r)
     struct rule *r;
{
  register unsigned int i;
  register struct dep *d;

  for (i = 0; r->targets[i] != 0; ++i)
    {
      fputs (r->targets[i], stdout);
      if (r->targets[i + 1] != 0)
	putchar (' ');
      else
	putchar (':');
    }
  if (r->terminal)
    putchar (':');

  for (d = r->deps; d != 0; d = d->next)
    printf (" %s", dep_name (d));
  putchar ('\n');

  if (r->cmds != 0)
    print_commands (r->cmds);
}

void
print_rule_data_base ()
{
  register unsigned int rules, terminal;
  register struct rule *r;

  puts ("\n# Implicit Rules");

  rules = terminal = 0;
  for (r = pattern_rules; r != 0; r = r->next)
    {
      ++rules;

      putchar ('\n');
      print_rule (r);

      if (r->terminal)
	++terminal;
    }

  if (rules == 0)
    puts ("\n# No implicit rules.");
  else
    {
      printf ("\n# %u implicit rules, %u", rules, terminal);
#ifndef	NO_FLOAT
      printf (" (%.1f%%)", (double) terminal / (double) rules * 100.0);
#else
      {
	int f = (terminal * 1000 + 5) / rules;
	printf (" (%d.%d%%)", f/10, f%10);
      }
#endif
      puts (" terminal.");
    }

  if (num_pattern_rules != rules)
    fatal ("BUG: num_pattern_rules wrong!  %u != %u",
	   num_pattern_rules, rules);
}



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         implicit.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Implicit rule searching for GNU Make.
Copyright (C) 1988, 89, 90, 91, 92, 93, 94 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "rule.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "dep.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
#undef stderr
#define stderr stdout

static int pattern_search PARAMS ((struct file *file, int archive, unsigned int depth,
		unsigned int recursions));

/* For a FILE which has no commands specified, try to figure out some
   from the implicit pattern rules.
   Returns 1 if a suitable implicit rule was found,
   after modifying FILE to contain the appropriate commands and deps,
   or returns 0 if no implicit rule was found.  */

int
try_implicit_rule (file, depth)
     struct file *file;
     unsigned int depth;
{
  DEBUGPR ("Looking for an implicit rule for `%s'.\n");

  /* The order of these searches was previously reversed.  My logic now is
     that since the non-archive search uses more information in the target
     (the archive search omits the archive name), it is more specific and
     should come first.  */

  if (pattern_search (file, 0, depth, 0))
    return 1;

#ifndef	NO_ARCHIVES
  /* If this is an archive member reference, use just the
     archive member name to search for implicit rules.  */
  if (ar_name (file->name))
    {
      DEBUGPR ("Looking for archive-member implicit rule for `%s'.\n");
      if (pattern_search (file, 1, depth, 0))
	return 1;
    }
#endif

  return 0;
}

#define DEBUGP2(msg, a1, a2)						      \
  do {									      \
    if (debug_flag)							      \
      { print_spaces (depth); printf (msg, a1, a2); fflush (stdout); }	      \
  } while (0)

/* Search the pattern rules for a rule with an existing dependency to make
   FILE.  If a rule is found, the appropriate commands and deps are put in FILE
   and 1 is returned.  If not, 0 is returned.

   If ARCHIVE is nonzero, FILE->name is of the form "LIB(MEMBER)".  A rule for
   "(MEMBER)" will be searched for, and "(MEMBER)" will not be chopped up into
   directory and filename parts.

   If an intermediate file is found by pattern search, the intermediate file
   is set up as a target by the recursive call and is also made a dependency
   of FILE.

   DEPTH is used for debugging messages.  */

static int
pattern_search (file, archive, depth, recursions)
     struct file *file;
     int archive;
     unsigned int depth;
     unsigned int recursions;
{
  /* Filename we are searching for a rule for.  */
  char *filename = archive ? index (file->name, '(') : file->name;

  /* Length of FILENAME.  */
  unsigned int namelen = strlen (filename);

  /* The last slash in FILENAME (or nil if there is none).  */
  char *lastslash;

  /* This is a file-object used as an argument in
     recursive calls.  It never contains any data
     except during a recursive call.  */
  struct file *intermediate_file = 0;

  /* List of dependencies found recursively.  */
  struct file **intermediate_files
    = (struct file **) alloca (max_pattern_deps * sizeof (struct file *));

  /* List of the patterns used to find intermediate files.  */
  char **intermediate_patterns
    = (char **) alloca (max_pattern_deps * sizeof (char *));

  /* This buffer records all the dependencies actually found for a rule.  */
  char **found_files = (char **) alloca (max_pattern_deps * sizeof (char *));
  /* Number of dep names now in FOUND_FILES.  */
  unsigned int deps_found;

  /* Names of possible dependencies are constructed in this buffer.  */
  register char *depname = (char *) alloca (namelen + max_pattern_dep_length);

  /* The start and length of the stem of FILENAME for the current rule.  */
  register char *stem;
  register unsigned int stemlen;

  /* Buffer in which we store all the rules that are possibly applicable.  */
  struct rule **tryrules
    = (struct rule **) alloca (num_pattern_rules * max_pattern_targets
			       * sizeof (struct rule *));

  /* Number of valid elements in TRYRULES.  */
  unsigned int nrules;

  /* The numbers of the rule targets of each rule
     in TRYRULES that matched the target file.  */
  unsigned int *matches
    = (unsigned int *) alloca (num_pattern_rules * sizeof (unsigned int));

  /* Each element is nonzero if LASTSLASH was used in
     matching the corresponding element of TRYRULES.  */
  char *checked_lastslash
    = (char *) alloca (num_pattern_rules * sizeof (char));

  /* The index in TRYRULES of the rule we found.  */
  unsigned int foundrule;

  /* Nonzero if should consider intermediate files as dependencies.  */
  int intermed_ok;

  /* Nonzero if we have matched a pattern-rule target
     that is not just `%'.  */
  int specific_rule_matched = 0;

  register unsigned int i;
  register struct rule *rule;
  register struct dep *dep;

  char *p;

#ifndef	NO_ARCHIVES
  if (archive || ar_name (filename))
    lastslash = 0;
  else
#endif
    {
      /* Set LASTSLASH to point at the last slash in FILENAME
	 but not counting any slash at the end.  (foo/bar/ counts as
	 bar/ in directory foo/, not empty in directory foo/bar/.)  */
#ifdef VMS
      lastslash = rindex (filename, ']');
#else
      lastslash = rindex (filename, '/');
#endif
      if (lastslash != 0 && lastslash[1] == '\0')
	lastslash = 0;
    }

  /* First see which pattern rules match this target
     and may be considered.  Put them in TRYRULES.  */

  nrules = 0;
  for (rule = pattern_rules; rule != 0; rule = rule->next)
    {
      /* If the pattern rule has deps but no commands, ignore it.
	 Users cancel built-in rules by redefining them without commands.  */
      if (rule->deps != 0 && rule->cmds == 0)
	continue;

      /* If this rule is in use by a parent pattern_search,
	 don't use it here.  */
      if (rule->in_use)
	{
	  DEBUGP2 ("Avoiding implicit rule recursion.%s%s\n", "", "");
	  continue;
	}

      for (i = 0; rule->targets[i] != 0; ++i)
	{
	  char *target = rule->targets[i];
	  char *suffix = rule->suffixes[i];
	  int check_lastslash;

	  /* Rules that can match any filename and are not terminal
	     are ignored if we're recursing, so that they cannot be
	     intermediate files.  */
	  if (recursions > 0 && target[1] == '\0' && !rule->terminal)
	    continue;

	  if (rule->lens[i] > namelen)
	    /* It can't possibly match.  */
	    continue;

	  /* From the lengths of the filename and the pattern parts,
	     find the stem: the part of the filename that matches the %.  */
	  stem = filename + (suffix - target - 1);
	  stemlen = namelen - rule->lens[i] + 1;

	  /* Set CHECK_LASTSLASH if FILENAME contains a directory
	     prefix and the target pattern does not contain a slash.  */

#ifdef VMS
	  check_lastslash = lastslash != 0 && index (target, ']') == 0;
#else
	  check_lastslash = lastslash != 0 && index (target, '/') == 0;
#endif
	  if (check_lastslash)
	    {
	      /* In that case, don't include the
		 directory prefix in STEM here.  */
	      unsigned int difference = lastslash - filename + 1;
	      if (difference > stemlen)
		continue;
	      stemlen -= difference;
	      stem += difference;
	    }

	  /* Check that the rule pattern matches the text before the stem.  */
	  if (check_lastslash)
	    {
	      if (stem > (lastslash + 1)
		  && strncmp (target, lastslash + 1, stem - lastslash - 1))
		continue;
	    }
	  else if (stem > filename
		   && strncmp (target, filename, stem - filename))
	    continue;

	  /* Check that the rule pattern matches the text after the stem.
	     We could test simply use streq, but this way we compare the
	     first two characters immediately.  This saves time in the very
	     common case where the first character matches because it is a
	     period.  */
	  if (*suffix != stem[stemlen]
	      || (*suffix != '\0' && !streq (&suffix[1], &stem[stemlen + 1])))
	    continue;

	  /* Record if we match a rule that not all filenames will match.  */
	  if (target[1] != '\0')
	    specific_rule_matched = 1;

	  /* A rule with no dependencies and no commands exists solely to set
	     specific_rule_matched when it matches.  Don't try to use it.  */
	  if (rule->deps == 0 && rule->cmds == 0)
	    continue;

	  /* Record this rule in TRYRULES and the index of the matching
	     target in MATCHES.  If several targets of the same rule match,
	     that rule will be in TRYRULES more than once.  */
	  tryrules[nrules] = rule;
	  matches[nrules] = i;
	  checked_lastslash[nrules] = check_lastslash;
	  ++nrules;
	}
    }

  /* If we have found a matching rule that won't match all filenames,
     retroactively reject any non-"terminal" rules that do always match.  */
  if (specific_rule_matched)
    for (i = 0; i < nrules; ++i)
      if (!tryrules[i]->terminal)
	{
	  register unsigned int j;
	  for (j = 0; tryrules[i]->targets[j] != 0; ++j)
	    if (tryrules[i]->targets[j][1] == '\0')
	      break;
	  if (tryrules[i]->targets[j] != 0)
	    tryrules[i] = 0;
	}

  /* Try each rule once without intermediate files, then once with them.  */
  for (intermed_ok = 0; intermed_ok == !!intermed_ok; ++intermed_ok)
    {
      /* Try each pattern rule till we find one that applies.
	 If it does, copy the names of its dependencies (as substituted)
	 and store them in FOUND_FILES.  DEPS_FOUND is the number of them.  */

      for (i = 0; i < nrules; i++)
	{
	  int check_lastslash;

	  rule = tryrules[i];

	  /* RULE is nil when we discover that a rule,
	     already placed in TRYRULES, should not be applied.  */
	  if (rule == 0)
	    continue;

	  /* Reject any terminal rules if we're
	     looking to make intermediate files.  */
	  if (intermed_ok && rule->terminal)
	    continue;

	  /* Mark this rule as in use so a recursive
	     pattern_search won't try to use it.  */
	  rule->in_use = 1;

	  /* From the lengths of the filename and the matching pattern parts,
	     find the stem: the part of the filename that matches the %.  */
	  stem = filename
	    + (rule->suffixes[matches[i]] - rule->targets[matches[i]]) - 1;
	  stemlen = namelen - rule->lens[matches[i]] + 1;
	  check_lastslash = checked_lastslash[i];
	  if (check_lastslash)
	    {
	      stem += lastslash - filename + 1;
	      stemlen -= (lastslash - filename) + 1;
	    }

	  DEBUGP2 ("Trying pattern rule with stem `%.*s'.\n",
		   (int) stemlen, stem);

	  /* Try each dependency; see if it "exists".  */

	  deps_found = 0;
	  for (dep = rule->deps; dep != 0; dep = dep->next)
	    {
	      /* If the dependency name has a %, substitute the stem.  */
	      p = index (dep_name (dep), '%');
	      if (p != 0)
		{
		  register unsigned int i;
		  if (check_lastslash)
		    {
		      /* Copy directory name from the original FILENAME.  */
		      i = lastslash - filename + 1;
		      bcopy (filename, depname, i);
		    }
		  else
		    i = 0;
		  bcopy (dep_name (dep), depname + i, p - dep_name (dep));
		  i += p - dep_name (dep);
		  bcopy (stem, depname + i, stemlen);
		  i += stemlen;
		  strcpy (depname + i, p + 1);
		  p = depname;
		}
	      else
		p = dep_name (dep);

	      /* P is now the actual dependency name as substituted.  */

	      if (file_impossible_p (p))
		{
		  /* If this dependency has already been ruled
		     "impossible", then the rule fails and don't
		     bother trying it on the second pass either
		     since we know that will fail too.  */
		  DEBUGP2 ("Rejecting impossible %s dependency `%s'.\n",
			   p == depname ? "implicit" : "rule", p);
		  tryrules[i] = 0;
		  break;
		}

	      intermediate_files[deps_found] = 0;

	      DEBUGP2 ("Trying %s dependency `%s'.\n",
		       p == depname ? "implicit" : "rule", p);

	      /* The DEP->changed flag says that this dependency resides in a
		 nonexistent directory.  So we normally can skip looking for
		 the file.  However, if CHECK_LASTSLASH is set, then the
		 dependency file we are actually looking for is in a different
		 directory (the one gotten by prepending FILENAME's directory),
		 so it might actually exist.  */

	      if ((!dep->changed || check_lastslash)
		  && (lookup_file (p) != 0 || file_exists_p (p)))
		{
		  found_files[deps_found++] = savestring (p, strlen (p));
		  continue;
		}
	      /* This code, given FILENAME = "lib/foo.o", dependency name
		 "lib/foo.c", and VPATH=src, searches for "src/lib/foo.c".  */
	      if (vpath_search (&p, (time_t *) 0))
		{
		  DEBUGP2 ("Found dependency as `%s'.%s\n", p, "");
		  found_files[deps_found++] = p;
		  continue;
		}

	      /* We could not find the file in any place we should look.
		 Try to make this dependency as an intermediate file,
		 but only on the second pass.  */

	      if (intermed_ok)
		{
		  if (intermediate_file == 0)
		    intermediate_file
		      = (struct file *) alloca (sizeof (struct file));

		  DEBUGP2 ("Looking for a rule with %s file `%s'.\n",
			   "intermediate", p);

		  bzero ((char *) intermediate_file, sizeof (struct file));
		  intermediate_file->name = p;
		  if (pattern_search (intermediate_file, 0, depth + 1,
				      recursions + 1))
		    {
		      p = savestring (p, strlen (p));
		      intermediate_patterns[deps_found]
			= intermediate_file->name;
		      intermediate_file->name = p;
		      intermediate_files[deps_found] = intermediate_file;
		      intermediate_file = 0;
		      /* Allocate an extra copy to go in FOUND_FILES,
			 because every elt of FOUND_FILES is consumed
			 or freed later.  */
		      found_files[deps_found] = savestring (p, strlen (p));
		      ++deps_found;
		      continue;
		    }

		  /* If we have tried to find P as an intermediate
		     file and failed, mark that name as impossible
		     so we won't go through the search again later.  */
		  file_impossible (p);
		}

	      /* A dependency of this rule does not exist.
		 Therefore, this rule fails.  */
	      break;
	    }

	  /* This rule is no longer `in use' for recursive searches.  */
	  rule->in_use = 0;

	  if (dep != 0)
	    {
	      /* This pattern rule does not apply.
		 If some of its dependencies succeeded,
		 free the data structure describing them.  */
	      while (deps_found-- > 0)
		{
		  register struct file *f = intermediate_files[deps_found];
		  free (found_files[deps_found]);
		  if (f != 0
		      && (f->stem < f->name
			  || f->stem > f->name + strlen (f->name)))
		    free (f->stem);
		}
	    }
	  else
	    /* This pattern rule does apply.  Stop looking for one.  */
	    break;
	}

      /* If we found an applicable rule without
	 intermediate files, don't try with them.  */
      if (i < nrules)
	break;

      rule = 0;
    }

  /* RULE is nil if the loop went all the way
     through the list and everything failed.  */
  if (rule == 0)
    return 0;

  foundrule = i;

  /* If we are recursing, store the pattern that matched
     FILENAME in FILE->name for use in upper levels.  */

  if (recursions > 0)
    /* Kludge-o-matic */
    file->name = rule->targets[matches[foundrule]];

  /* FOUND_FILES lists the dependencies for the rule we found.
     This includes the intermediate files, if any.
     Convert them into entries on the deps-chain of FILE.  */

  while (deps_found-- > 0)
    {
      register char *s;

      if (intermediate_files[deps_found] != 0)
	{
	  /* If we need to use an intermediate file,
	     make sure it is entered as a target, with the info that was
	     found for it in the recursive pattern_search call.
	     We know that the intermediate file did not already exist as
	     a target; therefore we can assume that the deps and cmds
	     of F below are null before we change them.  */

	  struct file *imf = intermediate_files[deps_found];
	  register struct file *f = enter_file (imf->name);
	  f->deps = imf->deps;
	  f->cmds = imf->cmds;
	  f->stem = imf->stem;
	  imf = lookup_file (intermediate_patterns[deps_found]);
	  if (imf != 0 && imf->precious)
	    f->precious = 1;
	  f->intermediate = 1;
	  f->tried_implicit = 1;
	  for (dep = f->deps; dep != 0; dep = dep->next)
	    {
	      dep->file = enter_file (dep->name);
	      dep->name = 0;
	      dep->file->tried_implicit |= dep->changed;
	    }
	  num_intermediates++;
	}

      dep = (struct dep *) xmalloc (sizeof (struct dep));
      s = found_files[deps_found];
      if (recursions == 0)
	{
	  dep->name = 0;
	  dep->file = lookup_file (s);
	  if (dep->file == 0)
	    /* enter_file consumes S's storage.  */
	    dep->file = enter_file (s);
	  else
	    /* A copy of S is already allocated in DEP->file->name.
	       So we can free S.  */
	    free (s);
	}
      else
	{
	  dep->name = s;
	  dep->file = 0;
	  dep->changed = 0;
	}
      if (intermediate_files[deps_found] == 0 && tryrules[foundrule]->terminal)
	{
	  /* If the file actually existed (was not an intermediate file),
	     and the rule that found it was a terminal one, then we want
	     to mark the found file so that it will not have implicit rule
	     search done for it.  If we are not entering a `struct file' for
	     it now, we indicate this with the `changed' flag.  */
	  if (dep->file == 0)
	    dep->changed = 1;
	  else
	    dep->file->tried_implicit = 1;
	}
      dep->next = file->deps;
      file->deps = dep;
    }

  if (!checked_lastslash[foundrule])
    /* Always allocate new storage, since STEM might be
       on the stack for an intermediate file.  */
    file->stem = savestring (stem, stemlen);
  else
    {
      /* We want to prepend the directory from
	 the original FILENAME onto the stem.  */
      file->stem = (char *) xmalloc (((lastslash + 1) - filename)
				     + stemlen + 1);
      bcopy (filename, file->stem, (lastslash + 1) - filename);
      bcopy (stem, file->stem + ((lastslash + 1) - filename), stemlen);
      file->stem[((lastslash + 1) - filename) + stemlen] = '\0';
    }

  file->cmds = rule->cmds;

  /* Put the targets other than the one that
     matched into FILE's `also_make' member.  */

  /* If there was only one target, there is nothing to do.  */
  if (rule->targets[1] != 0)
    for (i = 0; rule->targets[i] != 0; ++i)
      if (i != matches[foundrule])
	{
	  struct dep *new = (struct dep *) xmalloc (sizeof (struct dep));
	  new->name = p = (char *) xmalloc (rule->lens[i] + stemlen + 1);
	  bcopy (rule->targets[i], p,
		 rule->suffixes[i] - rule->targets[i] - 1);
	  p += rule->suffixes[i] - rule->targets[i] - 1;
	  bcopy (stem, p, stemlen);
	  p += stemlen;
	  bcopy (rule->suffixes[i], p,
		 rule->lens[i] - (rule->suffixes[i] - rule->targets[i]) + 1);
	  new->file = enter_file (new->name);
	  new->next = file->also_make;
	  file->also_make = new;
	}


  return 1;
}



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         default.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Data base of default implicit rules for GNU Make.
Copyright (C) 1988,89,90,91,92,93,94,95,96 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "rule.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "dep.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
#include "job.h"
/* #include "commands.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "variable.h"  <- modification by J.Ruthruff, 7/20 */
#undef stderr
#define stderr stdout

/* Define GCC_IS_NATIVE if gcc is the native development environment on
   your system (gcc/bison/flex vs cc/yacc/lex).  */
#ifdef __MSDOS__
#define GCC_IS_NATIVE
#endif


/* This is the default list of suffixes for suffix rules.
   `.s' must come last, so that a `.o' file will be made from
   a `.c' or `.p' or ... file rather than from a .s file.  */

static char default_suffixes[]
#ifdef VMS
  = ".exe .olb .ln .obj .c .cc .pas .p .for .f .r .y .l .mar \
.mod .sym .def .h .info .dvi .tex .texinfo .texi .txinfo \
.w .ch .cweb .web .com .sh .elc .el";
#else
  = ".out .a .ln .o .c .cc .C .p .f .F .r .y .l .s .S \
.mod .sym .def .h .info .dvi .tex .texinfo .texi .txinfo \
.w .ch .web .sh .elc .el";
#endif

static struct pspec default_pattern_rules[] =
  {
    { "(%)", "%",
	"$(AR) $(ARFLAGS) $@ $<" },

    /* The X.out rules are only in BSD's default set because
       BSD Make has no null-suffix rules, so `foo.out' and
       `foo' are the same thing.  */
#ifdef VMS
    { "%.exe", "%",
        "copy $< $@" },
#else
    { "%.out", "%",
	"@rm -f $@ \n cp $< $@" },
#endif
    /* Syntax is "ctangle foo.w foo.ch foo.c".  */
    { "%.c", "%.w %.ch",
	"$(CTANGLE) $^ $@" },
    { "%.tex", "%.w %.ch",
	"$(CWEAVE) $^ $@" },

    { 0, 0, 0 }
  };

static struct pspec default_terminal_rules[] =
  {
#ifdef VMS
    /* RCS.  */
    { "%", "%$$5lv", /* Multinet style */
        "if f$$search($@) .nes. \"\" then +$(CHECKOUT,v)" },
    { "%", "[.$$rcs]%$$5lv", /* Multinet style */
        "if f$$search($@) .nes. \"\" then +$(CHECKOUT,v)" },
    { "%", "%_v", /* Normal style */
        "if f$$search($@) .nes. \"\" then +$(CHECKOUT,v)" },
    { "%", "[.rcs]%_v", /* Normal style */
        "if f$$search($@) .nes. \"\" then +$(CHECKOUT,v)" },

    /* SCCS.  */
	/* ain't no SCCS on vms */
#else
    /* RCS.  */
    { "%", "%,v",
	"$(CHECKOUT,v)" },
    { "%", "RCS/%,v",
	"$(CHECKOUT,v)" },

    /* SCCS.  */
    { "%", "s.%",
	"$(GET) $(GFLAGS) $(SCCS_OUTPUT_OPTION) $<" },
    { "%", "SCCS/s.%",
	"$(GET) $(GFLAGS) $(SCCS_OUTPUT_OPTION) $<" },
#endif /* !VMS */
    { 0, 0, 0 }
  };

static char *default_suffix_rules[] =
  {
#ifdef VMS
    ".obj.exe",
    "$(LINK.obj) $^ $(LOADLIBES) $(LDLIBS) /exe=$@",
    ".mar.exe",
    "$(LINK.mar) $^ $(LOADLIBES) $(LDLIBS) /exe=$@",
    ".c.exe",
    "$(COMPILE.c) $^ \n $(LINK.obj) $(subst .c,.obj,$^) $(LOADLIBES) $(LDLIBS) /exe=$@",
    ".cc.exe",
    "$(COMPILE.cc) $^ \n $(LINK.obj) $(subst .cc,.obj,$^) $(LOADLIBES) $(LDLIBS) /exe=$@",
    ".for.exe",
    "$(COMPILE.for) $^ \n $(LINK.obj) $(subst .for,.obj,$^) $(LOADLIBES) $(LDLIBS) /exe=$@",
    ".pas.exe",
    "$(COMPILE.pas) $^ \n $(LINK.obj) $(subst .pas,.obj,$^) $(LOADLIBES) $(LDLIBS) /exe=$@",

    ".com",
    "copy $< >$@",

    ".mar.obj",
    "$(COMPILE.mar) /obj=$@ $<",
    ".c.obj",
    "$(COMPILE.c) /obj=$@ $<",
    ".cc.obj",
    "$(COMPILE.cc) /obj=$@ $<",
    ".for.obj",
    "$(COMPILE.for) /obj=$@ $<",
    ".pas.obj",
    "$(COMPILE.pas) /obj=$@ $<",

    ".y.c",
    "$(YACC.y) $< \n rename y_tab.c $@",
    ".l.c",
    "$(LEX.l) $< \n rename lexyy.c $@",

    ".texinfo.info",
    "$(MAKEINFO) $<",

    ".tex.dvi",
    "$(TEX) $<",

#else /* ! VMS */

    ".o",
    "$(LINK.o) $^ $(LOADLIBES) $(LDLIBS) -o $@",
    ".s",
    "$(LINK.s) $^ $(LOADLIBES) $(LDLIBS) -o $@",
    ".S",
    "$(LINK.S) $^ $(LOADLIBES) $(LDLIBS) -o $@",
    ".c",
    "$(LINK.c) $^ $(LOADLIBES) $(LDLIBS) -o $@",
    ".cc",
    "$(LINK.cc) $^ $(LOADLIBES) $(LDLIBS) -o $@",
    ".C",
    "$(LINK.C) $^ $(LOADLIBES) $(LDLIBS) -o $@",
    ".f",
    "$(LINK.f) $^ $(LOADLIBES) $(LDLIBS) -o $@",
    ".p",
    "$(LINK.p) $^ $(LOADLIBES) $(LDLIBS) -o $@",
    ".F",
    "$(LINK.F) $^ $(LOADLIBES) $(LDLIBS) -o $@",
    ".r",
    "$(LINK.r) $^ $(LOADLIBES) $(LDLIBS) -o $@",
    ".mod",
    "$(COMPILE.mod) -o $@ -e $@ $^",

    ".def.sym",
    "$(COMPILE.def) -o $@ $<",

    ".sh",
    "cat $< >$@ \n chmod a+x $@",

    ".s.o",
    "$(COMPILE.s) -o $@ $<",
    ".S.o",
    "$(COMPILE.S) -o $@ $<",
    ".c.o",
    "$(COMPILE.c) $< $(OUTPUT_OPTION)",
    ".cc.o",
    "$(COMPILE.cc) $< $(OUTPUT_OPTION)",
    ".C.o",
    "$(COMPILE.C) $< $(OUTPUT_OPTION)",
    ".f.o",
    "$(COMPILE.f) $< $(OUTPUT_OPTION)",
    ".p.o",
    "$(COMPILE.p) $< $(OUTPUT_OPTION)",
    ".F.o",
    "$(COMPILE.F) $< $(OUTPUT_OPTION)",
    ".r.o",
    "$(COMPILE.r) $< $(OUTPUT_OPTION)",
    ".mod.o",
    "$(COMPILE.mod) -o $@ $<",

    ".c.ln",
    "$(LINT.c) -C$* $<",
    ".y.ln",
#ifndef __MSDOS__
    "$(YACC.y) $< \n $(LINT.c) -C$* y.tab.c \n $(RM) y.tab.c",
#else
    "$(YACC.y) $< \n $(LINT.c) -C$* y_tab.c \n $(RM) y_tab.c",
#endif
    ".l.ln",
    "@$(RM) $*.c\n $(LEX.l) $< > $*.c\n$(LINT.c) -i $*.c -o $@\n $(RM) $*.c",

    ".y.c",
#ifndef __MSDOS__
    "$(YACC.y) $< \n mv -f y.tab.c $@",
#else
    "$(YACC.y) $< \n mv -f y_tab.c $@",
#endif
    ".l.c",
    "@$(RM) $@ \n $(LEX.l) $< > $@",

    ".F.f",
    "$(PREPROCESS.F) $< $(OUTPUT_OPTION)",
    ".r.f",
    "$(PREPROCESS.r) $< $(OUTPUT_OPTION)",

    /* This might actually make lex.yy.c if there's no %R%
       directive in $*.l, but in that case why were you
       trying to make $*.r anyway?  */
    ".l.r",
    "$(LEX.l) $< > $@ \n mv -f lex.yy.r $@",

    ".S.s",
    "$(PREPROCESS.S) $< > $@",

    ".texinfo.info",
    "$(MAKEINFO) $(MAKEINFO_FLAGS) $< -o $@",

    ".texi.info",
    "$(MAKEINFO) $(MAKEINFO_FLAGS) $< -o $@",

    ".txinfo.info",
    "$(MAKEINFO) $(MAKEINFO_FLAGS) $< -o $@",

    ".tex.dvi",
    "$(TEX) $<",

    ".texinfo.dvi",
    "$(TEXI2DVI) $(TEXI2DVI_FLAGS) $<",

    ".texi.dvi",
    "$(TEXI2DVI) $(TEXI2DVI_FLAGS) $<",

    ".txinfo.dvi",
    "$(TEXI2DVI) $(TEXI2DVI_FLAGS) $<",

    ".w.c",
    "$(CTANGLE) $< - $@",	/* The `-' says there is no `.ch' file.  */

    ".web.p",
    "$(TANGLE) $<",

    ".w.tex",
    "$(CWEAVE) $< - $@",	/* The `-' says there is no `.ch' file.  */

    ".web.tex",
    "$(WEAVE) $<",

#endif /* !VMS */

    0, 0,
  };

static char *default_variables[] =
  {
#ifdef VMS
    "AR", "library/obj",
    "ARFLAGS", "/replace",
    "AS", "macro",
    "CC", "cc",
    "C++", "gcc/plus",
    "CXX", "gcc/plus",
    "CO", "co",
    "CPP", "$(CC) /preprocess_only",
    "FC", "fortran",
    /* System V uses these, so explicit rules using them should work.
       However, there is no way to make implicit rules use them and FC.  */
    "F77", "$(FC)",
    "F77FLAGS", "$(FFLAGS)",
    "LD", "link",
    "LEX", "lex",
    "PC", "pascal",
    "YACC", "yacc",	/* Or "bison -y"  */
    "MAKEINFO", "makeinfo",
    "TEX", "tex",
    "TEXINDEX", "texindex",

    "RM", "delete/nolog",

    "LINK.obj", "$(LD) $(LDFLAGS)",
    "COMPILE.c", "$(CC) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH)",
    "COMPILE.cc", "$(C++) $(C++FLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c",
    "YACC.y", "$(YACC) $(YFLAGS)",
    "LEX.l", "$(LEX) $(LFLAGS)",
    "COMPILE.for", "$(FC) $(FFLAGS) $(TARGET_ARCH)",
    "COMPILE.pas", "$(PC) $(PFLAGS) $(CPPFLAGS) $(TARGET_ARCH)",
    "COMPILE.mar", "$(AS) $(ASFLAGS) $(TARGET_MACH)",
    "LINT.c", "$(LINT) $(LINTFLAGS) $(CPPFLAGS) $(TARGET_ARCH)",

    "MV", "rename/new_version",
    "CP", "copy",

#else /* !VMS */

    "AR", "ar",
    "ARFLAGS", "rv",
    "AS", "as",
#ifdef GCC_IS_NATIVE
    "CC", "gcc",
    "CXX", "gcc",
#else
    "CC", "cc",
    "CXX", "g++",
#endif

    /* This expands to $(CO) $(COFLAGS) $< $@ if $@ does not exist,
       and to the empty string if $@ does exist.  */
    "CHECKOUT,v",
    "+$(patsubst $@-noexist,$(CO) $(COFLAGS) $< $@,\
		 $(filter-out $@,$(firstword $(wildcard $@) $@-noexist)))",

    "CO", "co",
    "CPP", "$(CC) -E",
#ifdef	CRAY
    "CF77PPFLAGS", "-P",
    "CF77PP", "/lib/cpp",
    "CFT", "cft77",
    "CF", "cf77",
    "FC", "$(CF)",
#else	/* Not CRAY.  */
#ifdef	_IBMR2
    "FC", "xlf",
#else
#ifdef	__convex__
    "FC", "fc",
#else
    "FC", "f77",
#endif /* __convex__ */
#endif /* _IBMR2 */
    /* System V uses these, so explicit rules using them should work.
       However, there is no way to make implicit rules use them and FC.  */
    "F77", "$(FC)",
    "F77FLAGS", "$(FFLAGS)",
#endif	/* Cray.  */
    "GET", SCCS_GET,
    "LD", "ld",
#ifdef GCC_IS_NATIVE
    "LEX", "flex",
#else
    "LEX", "lex",
#endif
    "LINT", "lint",
    "M2C", "m2c",
#ifdef	pyr
    "PC", "pascal",
#else
#ifdef	CRAY
    "PC", "PASCAL",
    "SEGLDR", "segldr",
#else
    "PC", "pc",
#endif	/* CRAY.  */
#endif	/* pyr.  */
#ifdef GCC_IS_NATIVE
    "YACC", "bison -y",
#else
    "YACC", "yacc",	/* Or "bison -y"  */
#endif
    "MAKEINFO", "makeinfo",
    "TEX", "tex",
    "TEXI2DVI", "texi2dvi",
    "WEAVE", "weave",
    "CWEAVE", "cweave",
    "TANGLE", "tangle",
    "CTANGLE", "ctangle",

    "RM", "rm -f",

    "LINK.o", "$(CC) $(LDFLAGS) $(TARGET_ARCH)",
    "COMPILE.c", "$(CC) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c",
    "LINK.c", "$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH)",
    "COMPILE.cc", "$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c",
    "COMPILE.C", "$(COMPILE.cc)",
    "LINK.cc", "$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH)",
    "LINK.C", "$(LINK.cc)",
    "YACC.y", "$(YACC) $(YFLAGS)",
    "LEX.l", "$(LEX) $(LFLAGS) -t",
    "COMPILE.f", "$(FC) $(FFLAGS) $(TARGET_ARCH) -c",
    "LINK.f", "$(FC) $(FFLAGS) $(LDFLAGS) $(TARGET_ARCH)",
    "COMPILE.F", "$(FC) $(FFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c",
    "LINK.F", "$(FC) $(FFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH)",
    "COMPILE.r", "$(FC) $(FFLAGS) $(RFLAGS) $(TARGET_ARCH) -c",
    "LINK.r", "$(FC) $(FFLAGS) $(RFLAGS) $(LDFLAGS) $(TARGET_ARCH)",
    "COMPILE.def", "$(M2C) $(M2FLAGS) $(DEFFLAGS) $(TARGET_ARCH)",
    "COMPILE.mod", "$(M2C) $(M2FLAGS) $(MODFLAGS) $(TARGET_ARCH)",
    "COMPILE.p", "$(PC) $(PFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c",
    "LINK.p", "$(PC) $(PFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH)",
    "LINK.s", "$(CC) $(ASFLAGS) $(LDFLAGS) $(TARGET_MACH)",
    "COMPILE.s", "$(AS) $(ASFLAGS) $(TARGET_MACH)",
    "LINK.S", "$(CC) $(ASFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_MACH)",
    "COMPILE.S", "$(CC) $(ASFLAGS) $(CPPFLAGS) $(TARGET_MACH) -c",
    "PREPROCESS.S", "$(CC) -E $(CPPFLAGS)",
    "PREPROCESS.F", "$(FC) $(FFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -F",
    "PREPROCESS.r", "$(FC) $(FFLAGS) $(RFLAGS) $(TARGET_ARCH) -F",
    "LINT.c", "$(LINT) $(LINTFLAGS) $(CPPFLAGS) $(TARGET_ARCH)",

#ifndef	NO_MINUS_C_MINUS_O
    "OUTPUT_OPTION", "-o $@",
#endif

#ifdef	SCCS_GET_MINUS_G
    "SCCS_OUTPUT_OPTION", "-G$@",
#endif

#endif /* !VMS */
    0, 0
  };

/* Set up the default .SUFFIXES list.  */

void
set_default_suffixes ()
{
  suffix_file = enter_file (".SUFFIXES");

  if (no_builtin_rules_flag)
    (void) define_variable ("SUFFIXES", 8, "", o_default, 0);
  else
    {
      char *p = default_suffixes;
      suffix_file->deps = (struct dep *)
	multi_glob (parse_file_seq (&p, '\0', sizeof (struct dep), 1),
		    sizeof (struct dep));
      (void) define_variable ("SUFFIXES", 8, default_suffixes, o_default, 0);
    }
}

/* Enter the default suffix rules as file rules.  This used to be done in
   install_default_implicit_rules, but that loses because we want the
   suffix rules installed before reading makefiles, and thee pattern rules
   installed after.  */

void
install_default_suffix_rules ()
{
  register char **s;

  if (no_builtin_rules_flag)
    return;

 for (s = default_suffix_rules; *s != 0; s += 2)
    {
      register struct file *f = enter_file (s[0]);
      /* Don't clobber cmds given in a makefile if there were any.  */
      if (f->cmds == 0)
	{
	  f->cmds = (struct commands *) xmalloc (sizeof (struct commands));
	  f->cmds->filename = 0;
	  f->cmds->commands = s[1];
	  f->cmds->command_lines = 0;
	}
    }
}


/* Install the default pattern rules.  */

void
install_default_implicit_rules ()
{
  register struct pspec *p;

  if (no_builtin_rules_flag)
    return;

  for (p = default_pattern_rules; p->target != 0; ++p)
    install_pattern_rule (p, 0);

  for (p = default_terminal_rules; p->target != 0; ++p)
    install_pattern_rule (p, 1);
}

void
define_default_variables ()
{
  register char **s;

  for (s = default_variables; *s != 0; s += 2)
    (void) define_variable (s[0], strlen (s[0]), s[1], o_default, 1);
}



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         variable.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Internals of variables for GNU Make.
Copyright (C) 1988, 89, 90, 91, 92, 93, 94, 96 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "dep.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
#include "job.h"
/* #include "commands.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "variable.h"  <- modification by J.Ruthruff, 7/20 */
#undef stderr
#define stderr stdout

#ifdef WIN32
#include "pathstuff.h"
#undef stderr
#define stderr stdout
#endif

/* Hash table of all global variable definitions.  */

#ifndef	VARIABLE_BUCKETS
#define VARIABLE_BUCKETS		523
#endif
#ifndef	PERFILE_VARIABLE_BUCKETS
#define	PERFILE_VARIABLE_BUCKETS	23
#endif
#ifndef	SMALL_SCOPE_VARIABLE_BUCKETS
#define	SMALL_SCOPE_VARIABLE_BUCKETS	13
#endif
static struct variable *variable_table[VARIABLE_BUCKETS];
static struct variable_set global_variable_set
  = { variable_table, VARIABLE_BUCKETS };
static struct variable_set_list global_setlist
  = { 0, &global_variable_set };
struct variable_set_list *current_variable_set_list = &global_setlist;

static struct variable *define_variable_in_set PARAMS ((char *name, unsigned int length,
							char *value, enum variable_origin origin,
							int recursive, struct variable_set *set));


/* Implement variables.  */

/* Define variable named NAME with value VALUE in SET.  VALUE is copied.
   LENGTH is the length of NAME, which does not need to be null-terminated.
   ORIGIN specifies the origin of the variable (makefile, command line
   or environment).
   If RECURSIVE is nonzero a flag is set in the variable saying
   that it should be recursively re-expanded.  */

static struct variable *
define_variable_in_set (name, length, value, origin, recursive, set)
     char *name;
     unsigned int length;
     char *value;
     enum variable_origin origin;
     int recursive;
     struct variable_set *set;
{
  register unsigned int i;
  register unsigned int hashval;
  register struct variable *v;

  hashval = 0;
  for (i = 0; i < length; ++i)
    HASH (hashval, name[i]);
  hashval %= set->buckets;

  for (v = set->table[hashval]; v != 0; v = v->next)
    if (*v->name == *name
	&& !strncmp (v->name + 1, name + 1, length - 1)
	&& v->name[length] == '\0')
      break;

  if (env_overrides && origin == o_env)
    origin = o_env_override;

  if (v != 0)
    {
      if (env_overrides && v->origin == o_env)
	/* V came from in the environment.  Since it was defined
	   before the switches were parsed, it wasn't affected by -e.  */
	v->origin = o_env_override;

      /* A variable of this name is already defined.
	 If the old definition is from a stronger source
	 than this one, don't redefine it.  */
      if ((int) origin >= (int) v->origin)
	{
	  if (v->value != 0)
	    free (v->value);
	  v->value = savestring (value, strlen (value));
	  v->origin = origin;
	  v->recursive = recursive;
	}
      return v;
    }

  /* Create a new variable definition and add it to the hash table.  */

  v = (struct variable *) xmalloc (sizeof (struct variable));
  v->name = savestring (name, length);
  v->value = savestring (value, strlen (value));
  v->origin = origin;
  v->recursive = recursive;
  v->expanding = 0;
  v->export = v_default;
  v->next = set->table[hashval];
  set->table[hashval] = v;
  return v;
}

/* Define a variable in the current variable set.  */

struct variable *
define_variable (name, length, value, origin, recursive)
     char *name;
     unsigned int length;
     char *value;
     enum variable_origin origin;
     int recursive;
{
  return define_variable_in_set (name, length, value, origin, recursive,
				 current_variable_set_list->set);
}

/* Define a variable in FILE's variable set.  */

struct variable *
define_variable_for_file (name, length, value, origin, recursive, file)
     char *name;
     unsigned int length;
     char *value;
     enum variable_origin origin;
     int recursive;
     struct file *file;
{
  return define_variable_in_set (name, length, value, origin, recursive,
				 file->variables->set);
}

/* Lookup a variable whose name is a string starting at NAME
   and with LENGTH chars.  NAME need not be null-terminated.
   Returns address of the `struct variable' containing all info
   on the variable, or nil if no such variable is defined.  */

struct variable *
lookup_variable (name, length)
     char *name;
     unsigned int length;
{
  register struct variable_set_list *setlist;

  register unsigned int i;
  register unsigned int rawhash = 0;

  for (i = 0; i < length; ++i)
    HASH (rawhash, name[i]);

  for (setlist = current_variable_set_list;
       setlist != 0; setlist = setlist->next)
    {
      register struct variable_set *set = setlist->set;
      register unsigned int hashval = rawhash % set->buckets;
      register struct variable *v;

      for (v = set->table[hashval]; v != 0; v = v->next)
	if (*v->name == *name
	    && !strncmp (v->name + 1, name + 1, length - 1)
	    && v->name[length] == 0)
	  return v;
    }

  return 0;
}

/* Initialize FILE's variable set list.  If FILE already has a variable set
   list, the topmost variable set is left intact, but the the rest of the
   chain is replaced with FILE->parent's setlist.  */

void
initialize_file_variables (file)
     struct file *file;
{
  register struct variable_set_list *l = file->variables;
  if (l == 0)
    {
      l = (struct variable_set_list *)
	xmalloc (sizeof (struct variable_set_list));
      l->set = (struct variable_set *) xmalloc (sizeof (struct variable_set));
      l->set->buckets = PERFILE_VARIABLE_BUCKETS;
      l->set->table = (struct variable **)
	xmalloc (l->set->buckets * sizeof (struct variable *));
      bzero ((char *) l->set->table,
	     l->set->buckets * sizeof (struct variable *));
      file->variables = l;
    }

  if (file->parent == 0)
    l->next = &global_setlist;
  else
    {
      if (file->parent->variables == 0)
	initialize_file_variables (file->parent);
      l->next = file->parent->variables;
    }
}

/* Pop the top set off the current variable set list,
   and free all its storage.  */

void
pop_variable_scope ()
{
  register struct variable_set_list *setlist = current_variable_set_list;
  register struct variable_set *set = setlist->set;
  register unsigned int i;

  current_variable_set_list = setlist->next;
  free ((char *) setlist);

  for (i = 0; i < set->buckets; ++i)
    {
      register struct variable *next = set->table[i];
      while (next != 0)
	{
	  register struct variable *v = next;
	  next = v->next;

	  free (v->name);
	  free ((char *) v);
	}
    }
  free ((char *) set->table);
  free ((char *) set);
}

/* Create a new variable set and push it on the current setlist.  */

void
push_new_variable_scope ()
{
  register struct variable_set_list *setlist;
  register struct variable_set *set;

  set = (struct variable_set *) xmalloc (sizeof (struct variable_set));
  set->buckets = SMALL_SCOPE_VARIABLE_BUCKETS;
  set->table = (struct variable **)
    xmalloc (set->buckets * sizeof (struct variable *));
  bzero ((char *) set->table, set->buckets * sizeof (struct variable *));

  setlist = (struct variable_set_list *)
    xmalloc (sizeof (struct variable_set_list));
  setlist->set = set;
  setlist->next = current_variable_set_list;
  current_variable_set_list = setlist;
}

/* Merge SET1 into SET0, freeing unused storage in SET1.  */

static void
merge_variable_sets (set0, set1)
     struct variable_set *set0, *set1;
{
  register unsigned int bucket1;

  for (bucket1 = 0; bucket1 < set1->buckets; ++bucket1)
    {
      register struct variable *v1 = set1->table[bucket1];
      while (v1 != 0)
	{
	  struct variable *next = v1->next;
	  unsigned int bucket0;
	  register struct variable *v0;

	  if (set1->buckets >= set0->buckets)
	    bucket0 = bucket1;
	  else
	    {
	      register char *n;
	      bucket0 = 0;
	      for (n = v1->name; *n != '\0'; ++n)
		HASH (bucket0, *n);
	    }
	  bucket0 %= set0->buckets;

	  for (v0 = set0->table[bucket0]; v0 != 0; v0 = v0->next)
	    if (streq (v0->name, v1->name))
	      break;

	  if (v0 == 0)
	    {
	      /* There is no variable in SET0 with the same name.  */
	      v1->next = set0->table[bucket0];
	      set0->table[bucket0] = v1;
	    }
	  else
	    {
	      /* The same variable exists in both sets.
		 SET0 takes precedence.  */
	      free (v1->value);
	      free ((char *) v1);
	    }

	  v1 = next;
	}
    }
}

/* Merge SETLIST1 into SETLIST0, freeing unused storage in SETLIST1.  */

void
merge_variable_set_lists (setlist0, setlist1)
     struct variable_set_list **setlist0, *setlist1;
{
  register struct variable_set_list *list0 = *setlist0;
  struct variable_set_list *last0 = 0;

  while (setlist1 != 0 && list0 != 0)
    {
      struct variable_set_list *next = setlist1;
      setlist1 = setlist1->next;

      merge_variable_sets (list0->set, next->set);

      free ((char *) next);

      last0 = list0;
      list0 = list0->next;
    }

  if (setlist1 != 0)
    {
      if (last0 == 0)
	*setlist0 = setlist1;
      else
	last0->next = setlist1;
    }
}

/* Define the automatic variables, and record the addresses
   of their structures so we can change their values quickly.  */

void
define_automatic_variables ()
{
#ifdef WIN32
  extern char* default_shell;
#else
  extern char default_shell[];
#endif
  register struct variable *v;
  char buf[200];

  sprintf (buf, "%u", makelevel);
  (void) define_variable ("MAKELEVEL", 9, buf, o_env, 0);

  sprintf (buf, "%s%s%s",
	   version_string,
	   (remote_description == 0 || remote_description[0] == '\0')
	   ? "" : "-",
	   (remote_description == 0 || remote_description[0] == '\0')
	   ? "" : remote_description);
  (void) define_variable ("MAKE_VERSION", 12, buf, o_default, 0);


  /* This won't override any definition, but it
     will provide one if there isn't one there.  */
  v = define_variable ("SHELL", 5, default_shell, o_default, 0);
  v->export = v_export;		/* Always export SHELL.  */

  /* Don't let SHELL come from the environment.  */
  if (*v->value == '\0' || v->origin == o_env || v->origin == o_env_override)
    {
      free (v->value);
      v->origin = o_file;
      v->value = savestring (default_shell, strlen (default_shell));
    }

  /* Make sure MAKEFILES gets exported if it is set.  */
  v = define_variable ("MAKEFILES", 9, "", o_default, 0);
  v->export = v_ifset;

  /* Define the magic D and F variables in terms of
     the automatic variables they are variations of.  */

  define_variable ("@D", 2, "$(patsubst %/,%,$(dir $@))", o_automatic, 1);
  define_variable ("%D", 2, "$(patsubst %/,%,$(dir $%))", o_automatic, 1);
  define_variable ("*D", 2, "$(patsubst %/,%,$(dir $*))", o_automatic, 1);
  define_variable ("<D", 2, "$(patsubst %/,%,$(dir $<))", o_automatic, 1);
  define_variable ("?D", 2, "$(patsubst %/,%,$(dir $?))", o_automatic, 1);
  define_variable ("^D", 2, "$(patsubst %/,%,$(dir $^))", o_automatic, 1);
  define_variable ("+D", 2, "$(patsubst %/,%,$(dir $+))", o_automatic, 1);
  define_variable ("@F", 2, "$(notdir $@)", o_automatic, 1);
  define_variable ("%F", 2, "$(notdir $%)", o_automatic, 1);
  define_variable ("*F", 2, "$(notdir $*)", o_automatic, 1);
  define_variable ("<F", 2, "$(notdir $<)", o_automatic, 1);
  define_variable ("?F", 2, "$(notdir $?)", o_automatic, 1);
  define_variable ("^F", 2, "$(notdir $^)", o_automatic, 1);
  define_variable ("+F", 2, "$(notdir $+)", o_automatic, 1);
}

int export_all_variables;

/* Create a new environment for FILE's commands.
   If FILE is nil, this is for the `shell' function.
   The child's MAKELEVEL variable is incremented.  */

char **
target_environment (file)
     struct file *file;
{
  struct variable_set_list *set_list;
  register struct variable_set_list *s;
  struct variable_bucket
    {
      struct variable_bucket *next;
      struct variable *variable;
    };
  struct variable_bucket **table;
  unsigned int buckets;
  register unsigned int i;
  register unsigned nvariables;
  char **result;
  unsigned int mklev_hash;

  if (file == 0)
    set_list = current_variable_set_list;
  else
    set_list = file->variables;

  /* Find the lowest number of buckets in any set in the list.  */
  s = set_list;
  buckets = s->set->buckets;
  for (s = s->next; s != 0; s = s->next)
    if (s->set->buckets < buckets)
      buckets = s->set->buckets;

  /* Find the hash value of the bucket `MAKELEVEL' will fall into.  */
  {
    char *p = "MAKELEVEL";
    mklev_hash = 0;
    while (*p != '\0')
      HASH (mklev_hash, *p++);
  }

  /* Temporarily allocate a table with that many buckets.  */
  table = (struct variable_bucket **)
    alloca (buckets * sizeof (struct variable_bucket *));
  bzero ((char *) table, buckets * sizeof (struct variable_bucket *));

  /* Run through all the variable sets in the list,
     accumulating variables in TABLE.  */
  nvariables = 0;
  for (s = set_list; s != 0; s = s->next)
    {
      register struct variable_set *set = s->set;
      for (i = 0; i < set->buckets; ++i)
	{
	  register struct variable *v;
	  for (v = set->table[i]; v != 0; v = v->next)
	    {
	      unsigned int j = i % buckets;
	      register struct variable_bucket *ov;
	      register char *p = v->name;

	      if (i == mklev_hash % set->buckets
		  && streq (v->name, "MAKELEVEL"))
		/* Don't include MAKELEVEL because it will be
		   added specially at the end.  */
		continue;

	      switch (v->export)
		{
		case v_default:
		  if (v->origin == o_default || v->origin == o_automatic)
		    /* Only export default variables by explicit request.  */
		    continue;

		  if (! export_all_variables
		      && v->origin != o_command
		      && v->origin != o_env && v->origin != o_env_override)
		    continue;

		  if (*p != '_' && (*p < 'A' || *p > 'Z')
		      && (*p < 'a' || *p > 'z'))
		    continue;
		  for (++p; *p != '\0'; ++p)
		    if (*p != '_' && (*p < 'a' || *p > 'z')
			&& (*p < 'A' || *p > 'Z') && (*p < '0' || *p > '9'))
		      break;
		  if (*p != '\0')
		    continue;

		case v_export:
		  break;

		case v_noexport:
		  continue;

		case v_ifset:
		  if (v->origin == o_default)
		    continue;
		  break;
		}

	      for (ov = table[j]; ov != 0; ov = ov->next)
		if (streq (v->name, ov->variable->name))
		  break;
	      if (ov == 0)
		{
		  register struct variable_bucket *entry;
		  entry = (struct variable_bucket *)
		    alloca (sizeof (struct variable_bucket));
		  entry->next = table[j];
		  entry->variable = v;
		  table[j] = entry;
		  ++nvariables;
		}
	    }
	}
    }

  result = (char **) xmalloc ((nvariables + 2) * sizeof (char *));
  nvariables = 0;
  for (i = 0; i < buckets; ++i)
    {
      register struct variable_bucket *b;
      for (b = table[i]; b != 0; b = b->next)
	{
	  register struct variable *v = b->variable;
	  /* If V is recursively expanded and didn't come from the environment,
	     expand its value.  If it came from the environment, it should
	     go back into the environment unchanged.  */
	  if (v->recursive
	      && v->origin != o_env && v->origin != o_env_override)
	    {
	      char *value = recursively_expand (v);
#ifdef WIN32
              if (strcmp(v->name, "Path") == 0 ||
                  strcmp(v->name, "PATH") == 0)
                convert_Path_to_win32(value, ';');
#endif
	      result[nvariables++] = concat (v->name, "=", value);
	      free (value);
	    }
	  else
#ifdef WIN32
          {
            if (strcmp(v->name, "Path") == 0 ||
                strcmp(v->name, "PATH") == 0)
              convert_Path_to_win32(v->value, ';');
            result[nvariables++] = concat (v->name, "=", v->value);
          }
#else
	    result[nvariables++] = concat (v->name, "=", v->value);
#endif
	}
    }
  result[nvariables] = (char *) xmalloc (100);
  (void) sprintf (result[nvariables], "MAKELEVEL=%u", makelevel + 1);
  result[++nvariables] = 0;

  return result;
}

/* Try to interpret LINE (a null-terminated string) as a variable definition.

   ORIGIN may be o_file, o_override, o_env, o_env_override,
   or o_command specifying that the variable definition comes
   from a makefile, an override directive, the environment with
   or without the -e switch, or the command line.

   A variable definition has the form "name = value" or "name := value".
   Any whitespace around the "=" or ":=" is removed.  The first form
   defines a variable that is recursively re-evaluated.  The second form
   defines a variable whose value is variable-expanded at the time of
   definition and then is evaluated only once at the time of expansion.

   If a variable was defined, a pointer to its `struct variable' is returned.
   If not, NULL is returned.  */

struct variable *
try_variable_definition (filename, lineno, line, origin)
     char *filename;
     unsigned int lineno;
     char *line;
     enum variable_origin origin;
{
  register int c;
  register char *p = line;
  register char *beg;
  register char *end;
  enum { bogus, simple, recursive, append } flavor = bogus;
  char *name, *expanded_name, *value;
  struct variable *v;

  while (1)
    {
      c = *p++;
      if (c == '\0' || c == '#')
	return 0;
      if (c == '=')
	{
	  end = p - 1;
	  flavor = recursive;
	  break;
	}
      else if (c == ':')
	if (*p == '=')
	  {
	    end = p++ - 1;
	    flavor = simple;
	    break;
	  }
	else
	  /* A colon other than := is a rule line, not a variable defn.  */
	  return 0;
      else if (c == '+' && *p == '=')
	{
	  end = p++ - 1;
	  flavor = append;
	  break;
	}
      else if (c == '$')
	{
	  /* This might begin a variable expansion reference.  Make sure we
	     don't misrecognize chars inside the reference as =, := or +=.  */
	  char closeparen;
	  int count;
	  c = *p++;
	  if (c == '(')
	    closeparen = ')';
	  else if (c == '{')
	    closeparen = '}';
	  else
	    continue;		/* Nope.  */

	  /* P now points past the opening paren or brace.
	     Count parens or braces until it is matched.  */
	  count = 0;
	  for (; *p != '\0'; ++p)
	    {
	      if (*p == c)
		++count;
	      else if (*p == closeparen && --count < 0)
		{
		  ++p;
		  break;
		}
	    }
	}
    }

  beg = next_token (line);
  while (end > beg && isblank (end[-1]))
    --end;
  p = next_token (p);

  /* Expand the name, so "$(foo)bar = baz" works.  */
  name = (char *) alloca (end - beg + 1);
  bcopy (beg, name, end - beg);
  name[end - beg] = '\0';
  expanded_name = allocated_variable_expand (name);

  if (expanded_name[0] == '\0')
    {
      if (filename == 0)
	fatal ("empty variable name");
      else
	makefile_fatal (filename, lineno, "empty variable name");
    }

  /* Calculate the variable's new value in VALUE.  */

  switch (flavor)
    {
    case bogus:
      /* Should not be possible.  */
      abort ();
      return 0;
    case simple:
      /* A simple variable definition "var := value".  Expand the value.  */
      value = variable_expand (p);
      break;
    case recursive:
      /* A recursive variable definition "var = value".
	 The value is used verbatim.  */
      value = p;
      break;
    case append:
      /* An appending variable definition "var += value".
	 Extract the old value and append the new one.  */
      v = lookup_variable (expanded_name, strlen (expanded_name));
      if (v == 0)
	{
	  /* There was no old value.
	     This becomes a normal recursive definition.  */
	  value = p;
	  flavor = recursive;
	}
      else
	{
	  /* Paste the old and new values together in VALUE.  */

	  unsigned int oldlen, newlen;

	  if (v->recursive)
	    /* The previous definition of the variable was recursive.
	       The new value comes from the unexpanded old and new values.  */
	    flavor = recursive;
	  else
	    /* The previous definition of the variable was simple.
	       The new value comes from the old value, which was expanded
	       when it was set; and from the expanded new value.  */
	    p = variable_expand (p);

	  oldlen = strlen (v->value);
	  newlen = strlen (p);
	  value = (char *) alloca (oldlen + 1 + newlen + 1);
	  bcopy (v->value, value, oldlen);
	  value[oldlen] = ' ';
	  bcopy (p, &value[oldlen + 1], newlen + 1);
	}
    }

  v = define_variable (expanded_name, strlen (expanded_name),
		       value, origin, flavor == recursive);

  free (expanded_name);

  return v;
}

/* Print information for variable V, prefixing it with PREFIX.  */

static void
print_variable (v, prefix)
     register struct variable *v;
     char *prefix;
{
  char *origin;

  switch (v->origin)
    {
    case o_default:
      origin = "default";
      break;
    case o_env:
      origin = "environment";
      break;
    case o_file:
      origin = "makefile";
      break;
    case o_env_override:
      origin = "environment under -e";
      break;
    case o_command:
      origin = "command line";
      break;
    case o_override:
      origin = "`override' directive";
      break;
    case o_automatic:
      origin = "automatic";
      break;
    case o_invalid: case_coverage = case_coverage;
    default:
      abort ();
      break;
    }
  printf ("# %s\n", origin);

  fputs (prefix, stdout);

  /* Is this a `define'?  */
  if (v->recursive && index (v->value, '\n') != 0)
    printf ("define %s\n%s\nendef\n", v->name, v->value);
  else
    {
      register char *p;

      printf ("%s %s= ", v->name, v->recursive ? "" : ":");

      /* Check if the value is just whitespace.  */
      p = next_token (v->value);
      if (p != v->value && *p == '\0')
	/* All whitespace.  */
	printf ("$(subst ,,%s)", v->value);
      else if (v->recursive)
	fputs (v->value, stdout);
      else
	/* Double up dollar signs.  */
	for (p = v->value; *p != '\0'; ++p)
	  {
	    if (*p == '$')
	      putchar ('$');
	    putchar (*p);
	  }
      putchar ('\n');
    }
}


/* Print all the variables in SET.  PREFIX is printed before
   the actual variable definitions (everything else is comments).  */

static void
print_variable_set (set, prefix)
     register struct variable_set *set;
     char *prefix;
{
  register unsigned int i, nvariables, per_bucket;
  register struct variable *v;

  per_bucket = nvariables = 0;
  for (i = 0; i < set->buckets; ++i)
    {
      register unsigned int this_bucket = 0;

      for (v = set->table[i]; v != 0; v = v->next)
	{
	  ++this_bucket;
	  print_variable (v, prefix);
	}

      nvariables += this_bucket;
      if (this_bucket > per_bucket)
	per_bucket = this_bucket;
    }

  if (nvariables == 0)
    puts ("# No variables.");
  else
    {
      printf ("# %u variables in %u hash buckets.\n",
	      nvariables, set->buckets);
#ifndef	NO_FLOAT
      printf ("# average of %.1f variables per bucket, \
max %u in one bucket.\n",
	      (double) nvariables / (double) set->buckets,
	      per_bucket);
#else
      {
	int f = (nvariables * 1000 + 5) / set->buckets;
	printf ("# average of %d.%d variables per bucket, \
max %u in one bucket.\n",
	      f/10, f%10,
	      per_bucket);
      }
#endif
    }
}


/* Print the data base of variables.  */

void
print_variable_data_base ()
{
  puts ("\n# Variables\n");

  print_variable_set (&global_variable_set, "");
}


/* Print all the local variables of FILE.  */

void
print_file_variables (file)
     struct file *file;
{
  if (file->variables != 0)
    print_variable_set (file->variables->set, "# ");
}

#ifdef WIN32
void
sync_Path_environment(void)
{
    char* path = allocated_variable_expand("$(Path)");
    static char* environ_path = NULL;

    if (!path)
        return;

    /*
     * If done this before, don't leak memory unnecessarily.
     * Free the previous entry before allocating new one.
     */
    if (environ_path)
        free(environ_path);

    /*
     * Create something WIN32 world can grok
     */
    convert_Path_to_win32(path, ';');
    environ_path = concat("Path", "=", path);
    putenv(environ_path);
    free(path);
}
#endif



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         expand.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Variable expansion functions for GNU Make.
Copyright (C) 1988, 89, 91, 92, 93, 95 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
#include "job.h"
/* #include "commands.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "variable.h"  <- modification by J.Ruthruff, 7/20 */
#undef stderr
#define stderr stdout

/* The next two describe the variable output buffer.
   This buffer is used to hold the variable-expansion of a line of the
   makefile.  It is made bigger with realloc whenever it is too small.
   variable_buffer_length is the size currently allocated.
   variable_buffer is the address of the buffer.  */

static unsigned int variable_buffer_length;
static char *variable_buffer;

/* Subroutine of variable_expand and friends:
   The text to add is LENGTH chars starting at STRING to the variable_buffer.
   The text is added to the buffer at PTR, and the updated pointer into
   the buffer is returned as the value.  Thus, the value returned by
   each call to variable_buffer_output should be the first argument to
   the following call.  */

char *
variable_buffer_output (ptr, string, length)
     char *ptr, *string;
     unsigned int length;
{
  register unsigned int newlen = length + (ptr - variable_buffer);

  if (newlen > variable_buffer_length)
    {
      unsigned int offset = ptr - variable_buffer;
      variable_buffer_length = (newlen + 100 > 2 * variable_buffer_length
				? newlen + 100
				: 2 * variable_buffer_length);
      variable_buffer = (char *) xrealloc (variable_buffer,
					   variable_buffer_length);
      ptr = variable_buffer + offset;
    }

  bcopy (string, ptr, length);
  return ptr + length;
}

/* Return a pointer to the beginning of the variable buffer.  */

static char *
initialize_variable_output ()
{
  /* If we don't have a variable output buffer yet, get one.  */

  if (variable_buffer == 0)
    {
      variable_buffer_length = 200;
      variable_buffer = (char *) xmalloc (variable_buffer_length);
      variable_buffer[0] = '\0';
    }

  return variable_buffer;
}

/* Recursively expand V.  The returned string is malloc'd.  */

char *
recursively_expand (v)
     register struct variable *v;
{
  char *value;

  if (v->expanding)
    {
      /* Expanding V causes infinite recursion.  Lose.  */
      if (reading_filename == 0)
	fatal ("Recursive variable `%s' references itself (eventually)",
	       v->name);
      else
	makefile_fatal
	  (reading_filename, *reading_lineno_ptr,
	   "Recursive variable `%s' references itself (eventually)",
	   v->name);
    }

  v->expanding = 1;
  value = allocated_variable_expand (v->value);
  v->expanding = 0;

  return value;
}

/* Warn that NAME is an undefined variable.  */

#ifdef __GNUC__
__inline
#endif
static void
warn_undefined (name, length)
     char *name;
     unsigned int length;
{
  if (warn_undefined_variables_flag)
    {
      static const char warnmsg[] = "warning: undefined variable `%.*s'";
      if (reading_filename != 0)
	makefile_error (reading_filename, *reading_lineno_ptr,
			warnmsg, length, name);
      else
	error (warnmsg, length, name);
    }
}

/* Expand a simple reference to variable NAME, which is LENGTH chars long.  */

#ifdef __GNUC__
__inline
#endif
static char *
reference_variable (o, name, length)
     char *o;
     char *name;
     unsigned int length;
{
  register struct variable *v = lookup_variable (name, length);

  if (v == 0)
    warn_undefined (name, length);

  if (v != 0 && *v->value != '\0')
    {
      char *value = (v->recursive ? recursively_expand (v) : v->value);
      o = variable_buffer_output (o, value, strlen (value));
      if (v->recursive)
	free (value);
    }

  return o;
}

/* Scan LINE for variable references and expansion-function calls.
   Build in `variable_buffer' the result of expanding the references and calls.
   Return the address of the resulting string, which is null-terminated
   and is valid only until the next time this function is called.  */

char *
variable_expand (line)
     register char *line;
{
  register struct variable *v;
  register char *p, *o, *p1;

  p = line;
  o = initialize_variable_output ();

  while (1)
    {
      /* Copy all following uninteresting chars all at once to the
         variable output buffer, and skip them.  Uninteresting chars end
	 at the next $ or the end of the input.  */

      p1 = index (p, '$');

      o = variable_buffer_output (o, p, p1 != 0 ? p1 - p : strlen (p) + 1);

      if (p1 == 0)
	break;
      p = p1 + 1;

      /* Dispatch on the char that follows the $.  */

      switch (*p)
	{
	case '$':
	  /* $$ seen means output one $ to the variable output buffer.  */
	  o = variable_buffer_output (o, p, 1);
	  break;

	case '(': case_coverage = case_coverage;
	case '{':
	  /* $(...) or ${...} is the general case of substitution.  */
	  {
	    char openparen = *p;
	    char closeparen = (openparen == '(') ? ')' : '}';
	    register char *beg = p + 1;
	    int free_beg = 0;
	    char *op, *begp;
	    char *end, *colon;

	    op = o;
	    begp = p;
	    if (handle_function (&op, &begp))
	      {
		o = op;
		p = begp;
		break;
	      }

	    /* Is there a variable reference inside the parens or braces?
	       If so, expand it before expanding the entire reference.  */

	    end = index (beg, closeparen);
	    if (end == 0)
	      {
		/* Unterminated variable reference.  */
		if (reading_filename != 0)
		  makefile_fatal (reading_filename, *reading_lineno_ptr,
				  "unterminated variable reference");
		else
		  fatal ("unterminated variable reference");
	      }
	    p1 = lindex (beg, end, '$');
	    if (p1 != 0)
	      {
		/* BEG now points past the opening paren or brace.
		   Count parens or braces until it is matched.  */
		int count = 0;
		for (p = beg; *p != '\0'; ++p)
		  {
		    if (*p == openparen)
		      ++count;
		    else if (*p == closeparen && --count < 0)
		      break;
		  }
		/* If COUNT is >= 0, there were unmatched opening parens
		   or braces, so we go to the simple case of a variable name
		   such as `$($(a)'.  */
		if (count < 0)
		  {
		    beg = expand_argument (beg, p); /* Expand the name.  */
		    free_beg = 1; /* Remember to free BEG when finished.  */
		    end = index (beg, '\0');
		  }
	      }
	    else
	      /* Advance P to the end of this reference.  After we are
                 finished expanding this one, P will be incremented to
                 continue the scan.  */
	      p = end;

	    /* This is not a reference to a built-in function and
	       any variable references inside are now expanded.
	       Is the resultant text a substitution reference?  */

	    colon = lindex (beg, end, ':');
	    if (colon != 0)
	      {
		/* This looks like a substitution reference: $(FOO:A=B).  */
		char *subst_beg, *subst_end, *replace_beg, *replace_end;

		subst_beg = colon + 1;
		subst_end = index (subst_beg, '=');
		if (subst_end == 0)
		  /* There is no = in sight.  Punt on the substitution
		     reference and treat this as a variable name containing
		     a colon, in the code below.  */
		  colon = 0;
		else
		  {
		    replace_beg = subst_end + 1;
		    replace_end = end;

		    /* Extract the variable name before the colon
		       and look up that variable.  */
		    v = lookup_variable (beg, colon - beg);
		    if (v == 0)
		      warn_undefined (beg, colon - beg);

		    if (v != 0 && *v->value != '\0')
		      {
			char *value = (v->recursive ? recursively_expand (v)
				       : v->value);
			char *pattern, *percent;
			if (free_beg)
			  {
			    *subst_end = '\0';
			    pattern = subst_beg;
			  }
			else
			  {
			    pattern = (char *) alloca (subst_end - subst_beg
						       + 1);
			    bcopy (subst_beg, pattern, subst_end - subst_beg);
			    pattern[subst_end - subst_beg] = '\0';
			  }
			percent = find_percent (pattern);
			if (percent != 0)
			  {
			    char *replace;
			    if (free_beg)
			      {
				*replace_end = '\0';
				replace = replace_beg;
			      }
			    else
			      {
				replace = (char *) alloca (replace_end
							   - replace_beg
							   + 1);
				bcopy (replace_beg, replace,
				       replace_end - replace_beg);
				replace[replace_end - replace_beg] = '\0';
			      }

			    o = patsubst_expand (o, value, pattern, replace,
						 percent, (char *) 0);
			  }
			else
			  o = subst_expand (o, value,
					    pattern, replace_beg,
					    strlen (pattern),
					    end - replace_beg,
					    0, 1);
			if (v->recursive)
			  free (value);
		      }
		  }
	      }

	    if (colon == 0)
	      /* This is an ordinary variable reference.
		 Look up the value of the variable.  */
		o = reference_variable (o, beg, end - beg);

	  if (free_beg)
	    free (beg);
	  }
	  break;

	case '\0':
	  break;

	default:
	  if (isblank (p[-1]))
	    break;

	  /* A $ followed by a random char is a variable reference:
	     $a is equivalent to $(a).  */
	  {
	    /* We could do the expanding here, but this way
	       avoids code repetition at a small performance cost.  */
	    char name[5];
	    name[0] = '$';
	    name[1] = '(';
	    name[2] = *p;
	    name[3] = ')';
	    name[4] = '\0';
	    p1 = allocated_variable_expand (name);
	    o = variable_buffer_output (o, p1, strlen (p1));
	    free (p1);
	  }

	  break;
	}

      if (*p == '\0')
	break;
      else
	++p;
    }

  (void) variable_buffer_output (o, "", 1);
  return initialize_variable_output ();
}

/* Expand an argument for an expansion function.
   The text starting at STR and ending at END is variable-expanded
   into a null-terminated string that is returned as the value.
   This is done without clobbering `variable_buffer' or the current
   variable-expansion that is in progress.  */

char *
expand_argument (str, end)
     char *str, *end;
{
  char *tmp;

  if (*end == '\0')
    tmp = str;
  else
    {
      tmp = (char *) alloca (end - str + 1);
      bcopy (str, tmp, end - str);
      tmp[end - str] = '\0';
    }

  return allocated_variable_expand (tmp);
}

/* Expand LINE for FILE.  Error messages refer to the file and line where
   FILE's commands were found.  Expansion uses FILE's variable set list.  */

char *
variable_expand_for_file (line, file)
     char *line;
     register struct file *file;
{
  char *result;
  struct variable_set_list *save;

  if (file == 0)
    return variable_expand (line);

  save = current_variable_set_list;
  current_variable_set_list = file->variables;
  reading_filename = file->cmds->filename;
  reading_lineno_ptr = &file->cmds->lineno;
  result = variable_expand (line);
  current_variable_set_list = save;
  reading_filename = 0;
  reading_lineno_ptr = 0;

  return result;
}

/* Like variable_expand_for_file, but the returned string is malloc'd.
   This function is called a lot.  It wants to be efficient.  */

char *
allocated_variable_expand_for_file (line, file)
     char *line;
     struct file *file;
{
  char *value;

  char *obuf = variable_buffer;
  unsigned int olen = variable_buffer_length;

  variable_buffer = 0;

  value = variable_expand_for_file (line, file);

#if 0
  /* Waste a little memory and save time.  */
  value = xrealloc (value, strlen (value))
#endif

  variable_buffer = obuf;
  variable_buffer_length = olen;

  return value;
}



  /*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         function.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Variable function expansion for GNU Make.
Copyright (C) 1988, 89, 91, 92, 93, 94, 95, 96 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "variable.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "dep.h"  <- modification by J.Ruthruff, 7/20 */
#include "job.h"
/* #include "commands.h"  <- modification by J.Ruthruff, 7/20 */
#undef stderr
#define stderr stdout

#ifdef __MSDOS__
#include <process.h>
#include <fcntl.h>
#undef stderr
#define stderr stdout
#endif
#ifdef _AMIGA
#include "amiga.h"
#undef stderr
#define stderr stdout
#endif
#ifdef WIN32
#include <windows.h>
#include <io.h>
#include "sub_proc.h"
#undef stderr
#define stderr stdout
#endif

static char *string_glob PARAMS ((char *line));

/* Store into VARIABLE_BUFFER at O the result of scanning TEXT and replacing
   each occurrence of SUBST with REPLACE. TEXT is null-terminated.  SLEN is
   the length of SUBST and RLEN is the length of REPLACE.  If BY_WORD is
   nonzero, substitutions are done only on matches which are complete
   whitespace-delimited words.  If SUFFIX_ONLY is nonzero, substitutions are
   done only at the ends of whitespace-delimited words.  */

char *
subst_expand (o, text, subst, replace, slen, rlen, by_word, suffix_only)
     char *o;
     char *text;
     char *subst, *replace;
     unsigned int slen, rlen;
     int by_word, suffix_only;
{
  register char *t = text;
  register char *p;

  if (slen == 0 && !by_word && !suffix_only)
    {
      /* The first occurrence of "" in any string is its end.  */
      o = variable_buffer_output (o, t, strlen (t));
      if (rlen > 0)
	o = variable_buffer_output (o, replace, rlen);
      return o;
    }

  do
    {
      if ((by_word | suffix_only) && slen == 0)
	/* When matching by words, the empty string should match
	   the end of each word, rather than the end of the whole text.  */
	p = end_of_token (next_token (t));
      else
	{
	  p = sindex (t, 0, subst, slen);
	  if (p == 0)
	    {
	      /* No more matches.  Output everything left on the end.  */
	      o = variable_buffer_output (o, t, strlen (t));
	      return o;
	    }
	}

      /* Output everything before this occurrence of the string to replace.  */
      if (p > t)
	o = variable_buffer_output (o, t, p - t);

      /* If we're substituting only by fully matched words,
	 or only at the ends of words, check that this case qualifies.  */
      if ((by_word
	   && ((p > t && !isblank (p[-1]))
	       || (p[slen] != '\0' && !isblank (p[slen]))))
	  || (suffix_only
	      && (p[slen] != '\0' && !isblank (p[slen]))))
	/* Struck out.  Output the rest of the string that is
	   no longer to be replaced.  */
	o = variable_buffer_output (o, subst, slen);
      else if (rlen > 0)
	/* Output the replacement string.  */
	o = variable_buffer_output (o, replace, rlen);

      /* Advance T past the string to be replaced.  */
      t = p + slen;
    } while (*t != '\0');

  return o;
}


/* Store into VARIABLE_BUFFER at O the result of scanning TEXT
   and replacing strings matching PATTERN with REPLACE.
   If PATTERN_PERCENT is not nil, PATTERN has already been
   run through find_percent, and PATTERN_PERCENT is the result.
   If REPLACE_PERCENT is not nil, REPLACE has already been
   run through find_percent, and REPLACE_PERCENT is the result.  */

char *
patsubst_expand (o, text, pattern, replace, pattern_percent, replace_percent)
     char *o;
     char *text;
     register char *pattern, *replace;
     register char *pattern_percent, *replace_percent;
{
  unsigned int pattern_prepercent_len, pattern_postpercent_len;
  unsigned int replace_prepercent_len, replace_postpercent_len;
  char *t;
  unsigned int len;
  int doneany = 0;

  /* We call find_percent on REPLACE before checking PATTERN so that REPLACE
     will be collapsed before we call subst_expand if PATTERN has no %.  */
  if (replace_percent == 0)
    replace_percent = find_percent (replace);
  if (replace_percent != 0)
    {
      /* Record the length of REPLACE before and after the % so
	 we don't have to compute these lengths more than once.  */
      replace_prepercent_len = replace_percent - replace;
      replace_postpercent_len = strlen (replace_percent + 1);
    }
  else
    /* We store the length of the replacement
       so we only need to compute it once.  */
    replace_prepercent_len = strlen (replace);

  if (pattern_percent == 0)
    pattern_percent = find_percent (pattern);
  if (pattern_percent == 0)
    /* With no % in the pattern, this is just a simple substitution.  */
    return subst_expand (o, text, pattern, replace,
			 strlen (pattern), strlen (replace), 1, 0);

  /* Record the length of PATTERN before and after the %
     so we don't have to compute it more than once.  */
  pattern_prepercent_len = pattern_percent - pattern;
  pattern_postpercent_len = strlen (pattern_percent + 1);

  while ((t = find_next_token (&text, &len)) != 0)
    {
      int fail = 0;

      /* Is it big enough to match?  */
      if (len < pattern_prepercent_len + pattern_postpercent_len)
	fail = 1;

      /* Does the prefix match?  */
      if (!fail && pattern_prepercent_len > 0
	  && (*t != *pattern
	      || t[pattern_prepercent_len - 1] != pattern_percent[-1]
	      || strncmp (t + 1, pattern + 1, pattern_prepercent_len - 1)))
	fail = 1;

      /* Does the suffix match?  */
      if (!fail && pattern_postpercent_len > 0
	  && (t[len - 1] != pattern_percent[pattern_postpercent_len]
	      || t[len - pattern_postpercent_len] != pattern_percent[1]
	      || strncmp (&t[len - pattern_postpercent_len],
			  &pattern_percent[1], pattern_postpercent_len - 1)))
	fail = 1;

      if (fail)
	/* It didn't match.  Output the string.  */
	o = variable_buffer_output (o, t, len);
      else
	{
	  /* It matched.  Output the replacement.  */

	  /* Output the part of the replacement before the %.  */
	  o = variable_buffer_output (o, replace, replace_prepercent_len);

	  if (replace_percent != 0)
	    {
	      /* Output the part of the matched string that
		 matched the % in the pattern.  */
	      o = variable_buffer_output (o, t + pattern_prepercent_len,
					  len - (pattern_prepercent_len
						 + pattern_postpercent_len));
	      /* Output the part of the replacement after the %.  */
	      o = variable_buffer_output (o, replace_percent + 1,
					  replace_postpercent_len);
	    }
	}

      /* Output a space, but not if the replacement is "".  */
      if (fail || replace_prepercent_len > 0
	  || (replace_percent != 0 && len + replace_postpercent_len > 0))
	{
	  o = variable_buffer_output (o, " ", 1);
	  doneany = 1;
	}
    }
  if (doneany)
    /* Kill the last space.  */
    --o;

  return o;
}

/* Handle variable-expansion-time functions such as $(dir foo/bar) ==> foo/  */

/* These enumeration constants distinguish the
   various expansion-time built-in functions.  */

enum function
  {
    function_subst,
    function_addsuffix,
    function_addprefix,
    function_dir,
    function_notdir,
    function_suffix,
    function_basename,
    function_wildcard,
    function_firstword,
    function_word,
    function_words,
    function_findstring,
    function_strip,
    function_join,
    function_patsubst,
    function_filter,
    function_filter_out,
    function_foreach,
    function_sort,
    function_origin,
    function_shell,
    function_invalid
  };

/* Greater than the length of any function name.  */
#define MAXFUNCTIONLEN 11

/* The function names and lengths of names, for looking them up.  */

static struct
  {
    char *name;
    unsigned int len;
    enum function function;
  } function_table[] =
  {
    { "subst", 5, function_subst },
    { "addsuffix", 9, function_addsuffix },
    { "addprefix", 9, function_addprefix },
    { "dir", 3, function_dir },
    { "notdir", 6, function_notdir },
    { "suffix", 6, function_suffix },
    { "basename", 8, function_basename },
    { "wildcard", 8, function_wildcard },
    { "firstword", 9, function_firstword },
    { "word", 4, function_word },
    { "words", 5, function_words },
    { "findstring", 10, function_findstring },
    { "strip", 5, function_strip },
    { "join", 4, function_join },
    { "patsubst", 8, function_patsubst },
    { "filter", 6, function_filter },
    { "filter-out", 10, function_filter_out },
    { "foreach", 7, function_foreach },
    { "sort", 4, function_sort },
    { "origin", 6, function_origin },
    { "shell", 5, function_shell },
    { 0, 0, function_invalid }
  };

/* Return 1 if PATTERN matches WORD, 0 if not.  */

int
pattern_matches (pattern, percent, word)
     register char *pattern, *percent, *word;
{
  unsigned int sfxlen, wordlen;

  if (percent == 0)
    {
      unsigned int len = strlen (pattern) + 1;
      char *new = (char *) alloca (len);
      bcopy (pattern, new, len);
      pattern = new;
      percent = find_percent (pattern);
      if (percent == 0)
	return streq (pattern, word);
    }

  sfxlen = strlen (percent + 1);
  wordlen = strlen (word);

  if (wordlen < (percent - pattern) + sfxlen
      || strncmp (pattern, word, percent - pattern))
    return 0;

  return !strcmp (percent + 1, word + (wordlen - sfxlen));
}

int shell_function_pid = 0, shell_function_completed;

/* Perform the function specified by FUNCTION on the text at TEXT.
   END is points to the end of the argument text (exclusive).
   The output is written into VARIABLE_BUFFER starting at O.  */

/* Note this absorbs a semicolon and is safe to use in conditionals.  */
#define BADARGS(func)							      \
  if (reading_filename != 0)						      \
    makefile_fatal (reading_filename, *reading_lineno_ptr,		      \
		    "insufficient arguments to function `%s'", 		      \
		    func);						      \
  else									      \
    fatal ("insufficient arguments to function `%s'", func)

static char *
expand_function (o, function, text, end)
     char *o;
     enum function function;
     char *text;
     char *end;
{
  char *p, *p2, *p3;
  unsigned int i, len;
  int doneany = 0;
  int count;
  char endparen = *end, startparen = *end == ')' ? '(' : '{';

  switch (function)
    {
    default:
      abort ();
      break;

#ifndef VMS /* not supported for vms yet */
    case function_shell:
      {
#ifdef WIN32
        SECURITY_ATTRIBUTES saAttr;
        HANDLE hIn;
        HANDLE hErr;
        HANDLE hChildOutRd;
        HANDLE hChildOutWr;
        HANDLE hProcess;
#endif
	char **argv;
	char *error_prefix;
#ifndef _AMIGA
	char **envp;
	int pipedes[2];
	int pid;
#endif

	/* Expand the command line.  */
	text = expand_argument (text, end);

	/* Construct the argument list.  */
	argv = construct_command_argv (text,
				       (char **) NULL, (struct file *) 0);
	if (argv == 0)
	  break;

#ifndef _AMIGA
	/* Using a target environment for `shell' loses in cases like:
	   	export var = $(shell echo foobie)
	   because target_environment hits a loop trying to expand $(var)
	   to put it in the environment.  This is even more confusing when
	   var was not explicitly exported, but just appeared in the
	   calling environment.  */
#if 1
	envp = environ;
#else
	/* Construct the environment.  */
	envp = target_environment ((struct file *) 0);
#endif
#endif	/* Not Amiga.  */

	/* For error messages.  */
	if (reading_filename != 0)
	  {
	    error_prefix = (char *) alloca (strlen (reading_filename) + 100);
	    sprintf (error_prefix,
		     "%s:%u: ", reading_filename, *reading_lineno_ptr);
	  }
	else
	  error_prefix = "";

#if !defined(__MSDOS__) && !defined(_AMIGA)
# ifdef WIN32
        saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
        saAttr.bInheritHandle = TRUE;
        saAttr.lpSecurityDescriptor = NULL;

        if (DuplicateHandle(GetCurrentProcess(),
                            GetStdHandle(STD_INPUT_HANDLE),
                            GetCurrentProcess(),
                            &hIn,
                            0,
                            TRUE,
                            DUPLICATE_SAME_ACCESS) == FALSE) {
          fatal("create_child_process: DuplicateHandle(In) failed (e=%d)\n",
                GetLastError());
        }
        if (DuplicateHandle(GetCurrentProcess(),
                            GetStdHandle(STD_ERROR_HANDLE),
                            GetCurrentProcess(),
                            &hErr,
                            0,
                            TRUE,
                            DUPLICATE_SAME_ACCESS) == FALSE) {
          fatal("create_child_process: DuplicateHandle(Err) failed (e=%d)\n",
                GetLastError());
        }

        if (!CreatePipe(&hChildOutRd, &hChildOutWr, &saAttr, 0))
          fatal("CreatePipe() failed (e=%d)\n", GetLastError());

        hProcess = process_init_fd(hIn, hChildOutWr, hErr);

        if (!hProcess)
          fatal("expand_function: process_init_fd() failed\n");
        else
          process_register(hProcess);

        /* make sure that CreateProcess() has Path it needs */
        sync_Path_environment();

        if (!process_begin(hProcess, argv, envp, argv[0], NULL))
                pid = (int) hProcess;
        else
                fatal("expand_function: unable to launch process (e=%d)\n",
                      process_last_err(hProcess));

        /* set up to read data from child */
        pipedes[0] = _open_osfhandle((long) hChildOutRd, O_RDONLY);

        /* this will be closed almost right away */
        pipedes[1] = _open_osfhandle((long) hChildOutWr, O_APPEND);
# else /* WIN32 */
	if (pipe (pipedes) < 0)
	  {
	    perror_with_name (error_prefix, "pipe");
	    break;
	  }

	pid = vfork ();
	if (pid < 0)
	  perror_with_name (error_prefix, "fork");
	else if (pid == 0)
	  child_execute_job (0, pipedes[1], argv, envp);
	else
# endif /* WIN32 */
	  {
	    /* We are the parent.  */

	    char *buffer;
	    unsigned int maxlen;
	    int cc;

	    /* Free the storage only the child needed.  */
	    free (argv[0]);
	    free ((char *) argv);
#if 0
	    for (i = 0; envp[i] != 0; ++i)
	      free (envp[i]);
	    free ((char *) envp);
#endif

	    /* Record the PID for reap_children.  */
	    shell_function_pid = pid;
	    shell_function_completed = 0;


	    /* Set up and read from the pipe.  */

	    maxlen = 200;
	    buffer = (char *) xmalloc (maxlen + 1);

	    /* Close the write side of the pipe.  */
	    (void) close (pipedes[1]);

	    /* Read from the pipe until it gets EOF.  */
	    i = 0;
	    do
	      {
		if (i == maxlen)
		  {
		    maxlen += 512;
		    buffer = (char *) xrealloc (buffer, maxlen + 1);
		  }

		errno = 0;
		cc = read (pipedes[0], &buffer[i], maxlen - i);
		if (cc > 0)
		  i += cc;
	      }
#ifdef EINTR
	    while (cc > 0 || errno == EINTR);
#else
	    while (cc > 0);
#endif

	    /* Close the read side of the pipe.  */
	    (void) close (pipedes[0]);

	    /* Loop until child_handler sets shell_function_completed
	       to the status of our child shell.  */
	    while (shell_function_completed == 0)
	      reap_children (1, 0);

	    shell_function_pid = 0;

	    /* The child_handler function will set shell_function_completed
	       to 1 when the child dies normally, or to -1 if it
	       dies with status 127, which is most likely an exec fail.  */

	    if (shell_function_completed == -1)
	      {
		/* This most likely means that the execvp failed,
		   so we should just write out the error message
		   that came in over the pipe from the child.  */
		fputs (buffer, stderr);
		fflush (stderr);
	      }
	    else
	      {
		/* The child finished normally.  Replace all
		   newlines in its output with spaces, and put
		   that in the variable output buffer.  */
		if (i > 0)
		  {
		    if (buffer[i - 1] == '\n')
		      buffer[--i] = '\0';
		    else
		      buffer[i] = '\0';
		    p = buffer;
		    while ((p = index (p, '\n')) != 0)
		      *p++ = ' ';
		    o = variable_buffer_output (o, buffer, i);
		  }
	      }

	    free (buffer);
	  }
#else	/* MSDOS or Amiga */
#ifndef _AMIGA
         {
	   /* MS-DOS can't do fork, but it can do spawn.  However, this
	      means that we don't have an opportunity to reopen stdout to
	      trap it.  Thus, we save our own stdout onto a new descriptor
	      and dup a temp file's descriptor onto our stdout temporarily.
	      After we spawn the shell program, we dup our own stdout back
	      to the stdout descriptor.  The buffer reading is the same as
	      above, except that we're now reading from a file.  */

	   int save_stdout;
	   int child_stdout;
	   char tmp_output[FILENAME_MAX];
	   FILE *child_stream;
	   unsigned int maxlen = 200;
	   int cc;
	   char *buffer;

	   strcpy (tmp_output, "shXXXXXX");
	   mktemp (tmp_output);
	   child_stdout = open (tmp_output,
				O_WRONLY|O_CREAT|O_TRUNC|O_TEXT, 0644);
	   save_stdout = dup (1);
	   dup2 (child_stdout, 1);
	   spawnvp (P_WAIT, argv[0], argv);
	   dup2 (save_stdout, 1);
	   close (child_stdout);
	   close (save_stdout);

	   child_stdout = open (tmp_output, O_RDONLY|O_TEXT, 0644);

	   buffer = xmalloc (maxlen);
	   i = 0;
	   do
	     {
	       if (i == maxlen)
		 {
		   maxlen += 512;
		   buffer = (char *) xrealloc (buffer, maxlen + 1);
		 }

	       cc = read (child_stdout, &buffer[i], maxlen - i);
	       if (cc > 0)
		 i += cc;
	     } while (cc > 0);

	   close (child_stdout);
	   unlink (tmp_output);

	   if (i > 0)
	     {
	       if (buffer[i - 1] == '\n')
		 buffer[--i] = '\0';
	       else
		 buffer[i] = '\0';
	       p = buffer;
	       while ((p = index (p, '\n')) != 0)
		 *p++ = ' ';
	       o = variable_buffer_output (o, buffer, i);
	     }
	   free (buffer);
	 }
#else /* Amiga */
	 {
	   /* Amiga can't fork nor spawn, but I can start a program with
	      redirection of my choice. The rest is the same as above. */
#include <dos/dos.h>
#include <proto/dos.h>
#undef stderr
#define stderr stdout

	   BPTR child_stdout;
	   char tmp_output[FILENAME_MAX];
	   unsigned int maxlen = 200;
	   int cc;
	   char * buffer, * ptr;
	   char ** aptr;
	   int len = 0;

	   strcpy (tmp_output, "t:MakeshXXXXXXXX");
	   mktemp (tmp_output);
	   child_stdout = Open (tmp_output, MODE_NEWFILE);

	   for (aptr=argv; *aptr; aptr++)
	     {
	       len += strlen (*aptr) + 1;
	     }

	   buffer = xmalloc (len + 1);
	   ptr = buffer;

	   for (aptr=argv; *aptr; aptr++)
	     {
	       strcpy (ptr, *aptr);
	       ptr += strlen (ptr) + 1;
	       *ptr ++ = ' ';
	       *ptr = 0;
	     }

	   ptr[-1] = '\n';

	   Execute (buffer, NULL, child_stdout);
	   free (buffer);

	   Close (child_stdout);

	   child_stdout = Open (tmp_output, MODE_OLDFILE);

	   buffer = xmalloc (maxlen);
	   i = 0;
	   do
	     {
	       if (i == maxlen)
		 {
		   maxlen += 512;
		   buffer = (char *) xrealloc (buffer, maxlen + 1);
		 }

	       cc = Read (child_stdout, &buffer[i], maxlen - i);
	       if (cc > 0)
		 i += cc;
	     } while (cc > 0);

	   Close (child_stdout);
	   DeleteFile (tmp_output);

	   if (i > 0)
	     {
	       if (buffer[i - 1] == '\n')
		 buffer[--i] = '\0';
	       else
		 buffer[i] = '\0';
	       p = buffer;
	       while ((p = index (p, '\n')) != 0)
		 *p++ = ' ';
	       o = variable_buffer_output (o, buffer, i);
	     }
	   free (buffer);
	 }
#endif	/* Not Amiga.  */
#endif	/* MSDOS or Amiga.  */

	free (text);
	break;
      }
#endif /* !VMS */

    case function_origin:
      /* Expand the argument.  */
      text = expand_argument (text, end);

      {
	register struct variable *v = lookup_variable (text, strlen (text));
	if (v == 0)
	  o = variable_buffer_output (o, "undefined", 9);
	else
	  switch (v->origin)
	    {
	    default: case_coverage = case_coverage;
	    case o_invalid:
	      abort ();
	      break;
	    case o_default:
	      o = variable_buffer_output (o, "default", 7);
	      break;
	    case o_env:
	      o = variable_buffer_output (o, "environment", 11);
	      break;
	    case o_file:
	      o = variable_buffer_output (o, "file", 4);
	      break;
	    case o_env_override:
	      o = variable_buffer_output (o, "environment override", 20);
	      break;
	    case o_command:
	      o = variable_buffer_output (o, "command line", 12);
	      break;
	    case o_override:
	      o = variable_buffer_output (o, "override", 8);
	      break;
	    case o_automatic:
	      o = variable_buffer_output (o, "automatic", 9);
	      break;
	    }
      }

      free (text);
      break;

    case function_sort:
      /* Expand the argument.  */
      text = expand_argument (text, end);

      {
	char **words = (char **) xmalloc (10 * sizeof (char *));
	unsigned int nwords = 10;
	register unsigned int wordi = 0;
	char *t;

	/* Chop TEXT into words and put them in WORDS.  */
	t = text;
	while ((p = find_next_token (&t, &len)) != 0)
	  {
	    if (wordi >= nwords - 1)
	      {
		nwords *= 2;
		words = (char **) xrealloc ((char *) words,
					    nwords * sizeof (char *));
	      }
	    words[wordi++] = savestring (p, len);
	  }

	if (wordi > 0)
	  {
	    /* Now sort the list of words.  */
	    qsort ((char *) words, wordi, sizeof (char *), alpha_compare);

	    /* Now write the sorted list.  */
	    for (i = 0; i < wordi; ++i)
	      {
		len = strlen (words[i]);
		if (i == wordi - 1 || strlen (words[i + 1]) != len
		    || strcmp (words[i], words[i + 1]))
		  {
		    o = variable_buffer_output (o, words[i], len);
		    o = variable_buffer_output (o, " ", 1);
		  }
		free (words[i]);
	      }
	    /* Kill the last space.  */
	    --o;
	  }

	free ((char *) words);
      }

      free (text);
      break;

    case function_foreach:
      {
	/* Get three comma-separated arguments but
	   expand only the first two.  */
	char *var, *list;
	register struct variable *v;

	count = 0;
	for (p = text; p < end; ++p)
	  {
	    if (*p == startparen)
	      ++count;
	    else if (*p == endparen)
	      --count;
	    else if (*p == ',' && count <= 0)
	      break;
	  }
	if (p == end)
	  BADARGS ("foreach");
	var = expand_argument (text, p);

	p2 = p + 1;
	count = 0;
	for (p = p2; p < end; ++p)
	  {
	    if (*p == startparen)
	      ++count;
	    else if (*p == endparen)
	      --count;
	    else if (*p == ',' && count <= 0)
	      break;
	  }
	if (p == end)
	  BADARGS ("foreach");
	list = expand_argument (p2, p);

	++p;
	text = savestring (p, end - p);

	push_new_variable_scope ();
	v = define_variable (var, strlen (var), "", o_automatic, 0);
	p3 = list;
	while ((p = find_next_token (&p3, &len)) != 0)
	  {
	    char *result;
	    char save = p[len];
	    p[len] = '\0';
	    v->value = p;
	    result = allocated_variable_expand (text);
	    p[len] = save;

	    o = variable_buffer_output (o, result, strlen (result));
	    o = variable_buffer_output (o, " ", 1);
	    doneany = 1;
	    free (result);
	  }
	if (doneany)
	  /* Kill the last space.  */
	  --o;

	pop_variable_scope ();

	free (var);
	free (list);
	free (text);
      }
      break;

    case function_filter: case_coverage = case_coverage;
    case function_filter_out:
      {
	struct word
	  {
	    struct word *next;
	    char *word;
	    int matched;
	  } *words, *wordtail, *wp;

	/* Get two comma-separated arguments and expand each one.  */
	count = 0;
	for (p = text; p < end; ++p)
	  {
	    if (*p == startparen)
	      ++count;
	    else if (*p == endparen)
	      --count;
	    else if (*p == ',' && count <= 0)
	      break;
	  }
	if (p == end)
	  BADARGS (function == function_filter ? "filter" : "filter-out");
	p2 = expand_argument (text, p);

	text = expand_argument (p + 1, end);

	/* Chop TEXT up into words and then run each pattern through.  */
	words = wordtail = 0;
	p3 = text;
	while ((p = find_next_token (&p3, &len)) != 0)
	  {
	    struct word *w = (struct word *) alloca (sizeof (struct word));
	    if (words == 0)
	      words = w;
	    else
	      wordtail->next = w;
	    wordtail = w;

	    if (*p3 != '\0')
	      ++p3;
	    p[len] = '\0';
	    w->word = p;
	    w->matched = 0;
	  }

	if (words != 0)
	  {
	    wordtail->next = 0;

	    /* Run each pattern through the words, killing words.  */
	    p3 = p2;
	    while ((p = find_next_token (&p3, &len)) != 0)
	      {
		char *percent;
		char save = p[len];
		p[len] = '\0';

		percent = find_percent (p);
		for (wp = words; wp != 0; wp = wp->next)
		  wp->matched |= (percent == 0 ? streq (p, wp->word)
				  : pattern_matches (p, percent, wp->word));

		p[len] = save;
	      }

	    /* Output the words that matched (or didn't, for filter-out).  */
	    for (wp = words; wp != 0; wp = wp->next)
	      if (function == function_filter ? wp->matched : !wp->matched)
		{
		  o = variable_buffer_output (o, wp->word, strlen (wp->word));
		  o = variable_buffer_output (o, " ", 1);
		  doneany = 1;
		}
	    if (doneany)
	      /* Kill the last space.  */
	      --o;
	  }

	free (p2);
	free (text);
      }
      break;

    case function_patsubst:
      /* Get three comma-separated arguments and expand each one.  */
      count = 0;
      for (p = text; p < end; ++p)
	{
	  if (*p == startparen)
	    ++count;
	  else if (*p == endparen)
	    --count;
	  else if (*p == ',' && count <= 0)
	    break;
	}
      if (p == end)
	BADARGS ("patsubst");

      p2 = p;
      count = 0;
      for (++p; p < end; ++p)
	{
	  if (*p == startparen)
	    ++count;
	  else if (*p == endparen)
	    --count;
	  else if (*p == ',' && count <= 0)
	    break;
	}
      if (p == end)
	BADARGS ("patsubst");

      text = expand_argument (text, p2);
      p3 = expand_argument (p2 + 1, p);
      p2 = expand_argument (p + 1, end);

      o = patsubst_expand (o, p2, text, p3, (char *) 0, (char *) 0);

      free (text);
      free (p3);
      free (p2);
      break;

    case function_join:
      /* Get two comma-separated arguments and expand each one.  */
      count = 0;
      for (p = text; p < end; ++p)
	{
	  if (*p == startparen)
	    ++count;
	  else if (*p == endparen)
	    --count;
	  else if (*p == ',' && count <= 0)
	    break;
	}
      if (p == end)
	BADARGS ("join");
      text = expand_argument (text, p);

      p = expand_argument (p + 1, end);

      {
	/* Write each word of the first argument directly followed
	   by the corresponding word of the second argument.
	   If the two arguments have a different number of words,
	   the excess words are just output separated by blanks.  */
	register char *tp, *pp;
	p2 = text;
	p3 = p;
	do
	  {
	    unsigned int tlen, plen;

	    tp = find_next_token (&p2, &tlen);
	    if (tp != 0)
	      o = variable_buffer_output (o, tp, tlen);

	    pp = find_next_token (&p3, &plen);
	    if (pp != 0)
	      o = variable_buffer_output (o, pp, plen);

	    if (tp != 0 || pp != 0)
	      {
		o = variable_buffer_output (o, " ", 1);
		doneany = 1;
	      }
	  }
	while (tp != 0 || pp != 0);
	if (doneany)
	  /* Kill the last blank.  */
	  --o;
      }

      free (text);
      free (p);
      break;

    case function_strip:
      /* Expand the argument.  */
      text = expand_argument (text, end);

      p2 = text;
      while ((p = find_next_token (&p2, &i)) != 0)
	{
	  o = variable_buffer_output (o, p, i);
	  o = variable_buffer_output (o, " ", 1);
	  doneany = 1;
	}
      if (doneany)
	/* Kill the last space.  */
	--o;

      free (text);
      break;

    case function_wildcard:
      text = expand_argument (text, end);

#ifdef _AMIGA
      o = wildcard_expansion (text, o);
#else
      p = string_glob (text);
      o = variable_buffer_output (o, p, strlen (p));
#endif

      free (text);
      break;

    case function_subst:
      /* Get three comma-separated arguments and expand each one.  */
      count = 0;
      for (p = text; p < end; ++p)
	{
	  if (*p == startparen)
	    ++count;
	  else if (*p == endparen)
	    --count;
	  else if (*p == ',' && count <= 0)
	    break;
	}
      if (p == end)
	BADARGS ("subst");

      p2 = p;
      count = 0;
      for (++p; p < end; ++p)
	{
	  if (*p == startparen)
	    ++count;
	  else if (*p == endparen)
	    --count;
	  else if (*p == ',' && count <= 0)
	    break;
	}
      if (p == end)
	BADARGS ("subst");

      text = expand_argument (text, p2);
      p3 = expand_argument (p2 + 1, p);
      p2 = expand_argument (p + 1, end);

      o = subst_expand (o, p2, text, p3, strlen (text), strlen (p3), 0, 0);

      free (text);
      free (p3);
      free (p2);
      break;

    case function_firstword:
      /* Expand the argument.  */
      text = expand_argument (text, end);

      /* Find the first word in TEXT.  */
      p2 = text;
      p = find_next_token (&p2, &i);
      if (p != 0)
	o = variable_buffer_output (o, p, i);

      free (text);
      break;

    case function_word:
      /* Get two comma-separated arguments and expand each one.  */
      count = 0;
      for (p = text; p < end; ++p)
	{
	  if (*p == startparen)
	    ++count;
	  else if (*p == endparen)
	    --count;
	  else if (*p == ',' && count <= 0)
	    break;
	}
      if (p == end)
	BADARGS ("word");
      text = expand_argument (text, p);

      p3 = expand_argument (p + 1, end);

      /* Check the first argument.  */
      for (p2 = text; *p2 != '\0'; ++p2)
	if (*p2 < '0' || *p2 > '9')
	  {
	    if (reading_filename != 0)
	      makefile_fatal (reading_filename, *reading_lineno_ptr,
			      "non-numeric first argument to `word' function");
	    else
	      fatal ("non-numeric first argument to `word' function");
	  }

      i = (unsigned int) atoi (text);
      if (i == 0)
	{
	  if (reading_filename != 0)
	    makefile_fatal (reading_filename, *reading_lineno_ptr,
			    "the `word' function takes a one-origin \
index argument");
	  else
	    fatal ("the `word' function takes a one-origin index argument");
	}

      p2 = p3;
      while ((p = find_next_token (&p2, &len)) != 0)
	if (--i == 0)
	  break;
      if (i == 0)
	o = variable_buffer_output (o, p, len);

      free (text);
      free (p3);
      break;

    case function_words:
      /* Expand the argument.  */
      text = expand_argument (text, end);

      i = 0;
      p2 = text;
      while (find_next_token (&p2, (unsigned int *) 0) != 0)
	++i;

      {
	char buf[20];
	sprintf (buf, "%d", i);
	o = variable_buffer_output (o, buf, strlen (buf));
      }

      free (text);
      break;

    case function_findstring:
      /* Get two comma-separated arguments and expand each one.  */
      count = 0;
      for (p = text; p < end; ++p)
	{
	  if (*p == startparen)
	    ++count;
	  else if (*p == endparen)
	    --count;
	  else if (*p == ',' && count <= 0)
	    break;
	}
      if (p == end)
	BADARGS ("findstring");
      text = expand_argument (text, p);

      p = expand_argument (p + 1, end);

      /* Find the first occurrence of the first string in the second.  */
      i = strlen (text);
      if (sindex (p, 0, text, i) != 0)
	o = variable_buffer_output (o, text, i);

      free (p);
      free (text);
      break;

    case function_addsuffix: case_coverage = case_coverage;
    case function_addprefix:
      /* Get two comma-separated arguments and expand each one.  */
      count = 0;
      for (p = text; p < end; ++p)
	{
	  if (*p == startparen)
	    ++count;
	  else if (*p == endparen)
	    --count;
	  else if (*p == ',' && count <= 0)
	    break;
	}
      if (p == end)
	BADARGS (function == function_addsuffix ? "addsuffix" : "addprefix");
      text = expand_argument (text, p);
      i = strlen (text);

      p2 = expand_argument (p + 1, end);

      p3 = p2;
      while ((p = find_next_token (&p3, &len)) != 0)
	{
	  if (function == function_addprefix)
	    o = variable_buffer_output (o, text, i);
	  o = variable_buffer_output (o, p, len);
	  if (function == function_addsuffix)
	    o = variable_buffer_output (o, text, i);
	  o = variable_buffer_output (o, " ", 1);
	  doneany = 1;
	}
      if (doneany)
	/* Kill last space.  */
	--o;

      free (p2);
      free (text);
      break;

    case function_dir: case_coverage = case_coverage;
    case function_basename:
      /* Expand the argument.  */
      text = expand_argument (text, end);

      p3 = text;
      while ((p2 = find_next_token (&p3, &len)) != 0)
	{
	  p = p2 + len;
#ifdef VMS
	  while (p >= p2 && *p != (function == function_dir ? ']' : '.'))
#else
	  while (p >= p2 && *p != (function == function_dir ? '/' : '.'))
#endif
	    --p;
	  if (p >= p2)
	    {
	      if (function == function_dir)
		++p;
	      o = variable_buffer_output (o, p2, p - p2);
	    }
	  else if (function == function_dir)
#ifdef VMS
            o = variable_buffer_output (o, "[]", 2);
#else
#ifndef _AMIGA
            o = variable_buffer_output (o, "./", 2);
#else
	    /* o = o */; /* Just a nop... */
#endif /* AMIGA */
#endif /* !VMS */
	  else
	    /* The entire name is the basename.  */
	    o = variable_buffer_output (o, p2, len);

	  o = variable_buffer_output (o, " ", 1);
	  doneany = 1;
	}
      if (doneany)
	/* Kill last space.  */
	--o;

      free (text);
      break;

    case function_notdir: case_coverage = case_coverage;
    case function_suffix:
      /* Expand the argument.  */
      text = expand_argument (text, end);

      p3 = text;
      while ((p2 = find_next_token (&p3, &len)) != 0)
	{
	  p = p2 + len;
#ifdef VMS
	  while (p >= p2 && *p != (function == function_notdir ? ']' : '.'))
#else
	  while (p >= p2 && *p != (function == function_notdir ? '/' : '.'))
#endif
	    --p;
	  if (p >= p2)
	    {
	      if (function == function_notdir)
		++p;
	      o = variable_buffer_output (o, p, len - (p - p2));
	    }
	  else if (function == function_notdir)
	    o = variable_buffer_output (o, p2, len);

	  if (function == function_notdir || p >= p2)
	    {
	      o = variable_buffer_output (o, " ", 1);
	      doneany = 1;
	    }
	}
      if (doneany)
	/* Kill last space.  */
	--o;

      free (text);
      break;
    }

  return o;
}

/* Check for a function invocation in *STRINGP.  *STRINGP points at the
   opening ( or { and is not null-terminated.  If a function invocation
   is found, expand it into the buffer at *OP, updating *OP, incrementing
   *STRINGP past the reference and returning nonzero.  If not, return zero.  */

int
handle_function (op, stringp)
     char **op;
     char **stringp;

{
  register unsigned int code;
  unsigned int maxlen;
  char *beg = *stringp + 1;
  char *endref;

  endref = lindex (beg, beg + MAXFUNCTIONLEN, '\0');
  maxlen = endref != 0 ? endref - beg : MAXFUNCTIONLEN;

  for (code = 0; function_table[code].name != 0; ++code)
    {
      if (maxlen < function_table[code].len)
	continue;
      endref = beg + function_table[code].len;
      if (isblank (*endref)
	  && !strncmp (function_table[code].name, beg,
		       function_table[code].len))
	break;
    }
  if (function_table[code].name != 0)
    {
      /* We have found a call to an expansion-time function.
	 Find the end of the arguments, and do the function.  */

      char openparen = beg[-1], closeparen = openparen == '(' ? ')' : '}';
      int count = 0;
      char *argbeg;
      register char *p;

      /* Space after function name isn't part of the args.  */
      p = next_token (endref);
      argbeg = p;

      /* Count nested use of whichever kind of parens we use,
	 so that nested calls and variable refs work.  */

      for (; *p != '\0'; ++p)
	{
	  if (*p == openparen)
	    ++count;
	  else if (*p == closeparen && --count < 0)
	    break;
	}

      if (count >= 0)
	{
	  static const char errmsg[]
	    = "unterminated call to function `%s': missing `%c'";
	  if (reading_filename == 0)
	    fatal (errmsg, function_table[code].name, closeparen);
	  else
	    makefile_fatal (reading_filename, *reading_lineno_ptr, errmsg,
			    function_table[code].name, closeparen);
	}

      /* We found the end; expand the function call.  */

      *op = expand_function (*op, function_table[code].function, argbeg, p);
      *stringp = p;
      return 1;
    }

  return 0;
}

/* Glob-expand LINE.  The returned pointer is
   only good until the next call to string_glob.  */

static char *
string_glob (line)
     char *line;
{
  static char *result = 0;
  static unsigned int length;
  register struct nameseq *chain;
  register unsigned int idx;

  chain = multi_glob (parse_file_seq
		      (&line, '\0', sizeof (struct nameseq),
		       /* We do not want parse_file_seq to strip `./'s.
			  That would break examples like:
			  $(patsubst ./%.c,obj/%.o,$(wildcard ./*.c)).  */
		       0),
		      sizeof (struct nameseq));

  if (result == 0)
    {
      length = 100;
      result = (char *) xmalloc (100);
    }

  idx = 0;
  while (chain != 0)
    {
      register char *name = chain->name;
      unsigned int len = strlen (name);

      struct nameseq *next = chain->next;
      free ((char *) chain);
      chain = next;

      /* multi_glob will pass names without globbing metacharacters
	 through as is, but we want only files that actually exist.  */
      if (file_exists_p (name))
	{
	  if (idx + len + 1 > length)
	    {
	      length += (len + 1) * 2;
	      result = (char *) xrealloc (result, length);
	    }
	  bcopy (name, &result[idx], len);
	  idx += len;
	  result[idx++] = ' ';
	}

      free (name);
    }

  /* Kill the last space and terminate the string.  */
  if (idx == 0)
    result[0] = '\0';
  else
    result[idx - 1] = '\0';

  return result;
}



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         vpath.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Implementation of pattern-matching file search paths for GNU Make.
Copyright (C) 1988, 89, 91, 92, 93, 94, 95, 96 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "variable.h"  <- modification by J.Ruthruff, 7/20 */
#undef stderr
#define stderr stdout
#ifdef WIN32
#include "pathstuff.h"
#undef stderr
#define stderr stdout
#endif


/* Structure used to represent a selective VPATH searchpath.  */

struct vpath
  {
    struct vpath *next;	/* Pointer to next struct in the linked list.  */
    char *pattern;	/* The pattern to match.  */
    char *percent;	/* Pointer into `pattern' where the `%' is.  */
    unsigned int patlen;/* Length of the pattern.  */
    char **searchpath;	/* Null-terminated list of directories.  */
    unsigned int maxlen;/* Maximum length of any entry in the list.  */
  };

/* Linked-list of all selective VPATHs.  */

static struct vpath *vpaths;

/* Structure for the general VPATH given in the variable.  */

static struct vpath *general_vpath;

static int selective_vpath_search PARAMS ((struct vpath *path, char **file, time_t *mtime_ptr));

/* Reverse the chain of selective VPATH lists so they
   will be searched in the order given in the makefiles
   and construct the list from the VPATH variable.  */

void
build_vpath_lists ()
{
  register struct vpath *new = 0;
  register struct vpath *old, *nexto;
  register char *p;

  /* Reverse the chain.  */
  for (old = vpaths; old != 0; old = nexto)
    {
      nexto = old->next;
      old->next = new;
      new = old;
    }

  vpaths = new;

  /* If there is a VPATH variable with a nonnull value, construct the
     general VPATH list from it.  We use variable_expand rather than just
     calling lookup_variable so that it will be recursively expanded.  */

  {
    /* Turn off --warn-undefined-variables while we expand SHELL and IFS.  */
    int save = warn_undefined_variables_flag;
    warn_undefined_variables_flag = 0;

    p = variable_expand ("$(strip $(VPATH))");

    warn_undefined_variables_flag = save;
  }

  if (*p != '\0')
    {
      /* Save the list of vpaths.  */
      struct vpath *save_vpaths = vpaths;

      /* Empty `vpaths' so the new one will have no next, and `vpaths'
	 will still be nil if P contains no existing directories.  */
      vpaths = 0;

      /* Parse P.  */
      construct_vpath_list ("%", p);

      /* Store the created path as the general path,
	 and restore the old list of vpaths.  */
      general_vpath = vpaths;
      vpaths = save_vpaths;
    }
}

/* Construct the VPATH listing for the pattern and searchpath given.

   This function is called to generate selective VPATH lists and also for
   the general VPATH list (which is in fact just a selective VPATH that
   is applied to everything).  The returned pointer is either put in the
   linked list of all selective VPATH lists or in the GENERAL_VPATH
   variable.

   If SEARCHPATH is nil, remove all previous listings with the same
   pattern.  If PATTERN is nil, remove all VPATH listings.
   Existing and readable directories that are not "." given in the
   searchpath separated by colons are loaded into the directory hash
   table if they are not there already and put in the VPATH searchpath
   for the given pattern with trailing slashes stripped off if present
   (and if the directory is not the root, "/").
   The length of the longest entry in the list is put in the structure as well.
   The new entry will be at the head of the VPATHS chain.  */

void
construct_vpath_list (pattern, dirpath)
     char *pattern, *dirpath;
{
  register unsigned int elem;
  register char *p;
  register char **vpath;
  register unsigned int maxvpath;
  unsigned int maxelem;
  char *percent;

  if (pattern != 0)
    {
      pattern = savestring (pattern, strlen (pattern));
      percent = find_percent (pattern);
    }

  if (dirpath == 0)
    {
      /* Remove matching listings.  */
      register struct vpath *path, *lastpath;

      lastpath = 0;
      path = vpaths;
      while (path != 0)
	{
	  struct vpath *next = path->next;

	  if (pattern == 0
	      || (((percent == 0 && path->percent == 0)
		   || (percent - pattern == path->percent - path->pattern))
		  && streq (pattern, path->pattern)))
	    {
	      /* Remove it from the linked list.  */
	      if (lastpath == 0)
		vpaths = path->next;
	      else
		lastpath->next = next;

	      /* Free its unused storage.  */
	      free (path->pattern);
	      free ((char *) path->searchpath);
	      free ((char *) path);
	    }
	  else
	    lastpath = path;

	  path = next;
	}

      if (pattern != 0)
	free (pattern);
      return;
    }

#ifdef WIN32
    convert_vpath_to_win32(dirpath, ';');
#endif

  /* Figure out the maximum number of VPATH entries and
     put it in MAXELEM.  We start with 2, one before the
     first colon and one nil, the list terminator and
     increment our estimated number for each colon or blank we find.  */
  maxelem = 2;
  p = dirpath;
  while (*p != '\0')
    if (*p++ == PATH_SEPARATOR_CHAR || isblank (*p))
      ++maxelem;

  vpath = (char **) xmalloc (maxelem * sizeof (char *));
  maxvpath = 0;

  /* Skip over any initial colons and blanks.  */
  p = dirpath;
  while (*p == PATH_SEPARATOR_CHAR || isblank (*p))
    ++p;

  elem = 0;
  while (*p != '\0')
    {
      char *v;
      unsigned int len;

      /* Find the end of this entry.  */
      v = p;
      while (*p != '\0' && *p != PATH_SEPARATOR_CHAR && !isblank (*p))
	++p;

      len = p - v;
      /* Make sure there's no trailing slash,
	 but still allow "/" as a directory.  */
      if (len > 1 && p[-1] == '/')
	--len;

      if (len > 1 || *v != '.')
	{
	  v = savestring (v, len);

	  /* Verify that the directory actually exists.  */

	  if (dir_file_exists_p (v, ""))
	    {
	      /* It does.  Put it in the list.  */
	      vpath[elem++] = dir_name (v);
	      free (v);
	      if (len > maxvpath)
		maxvpath = len;
	    }
	  else
	    /* The directory does not exist.  Omit from the list.  */
	    free (v);
	}

      /* Skip over colons and blanks between entries.  */
      while (*p == PATH_SEPARATOR_CHAR || isblank (*p))
	++p;
    }

  if (elem > 0)
    {
      struct vpath *path;
      /* ELEM is now incremented one element past the last
	 entry, to where the nil-pointer terminator goes.
	 Usually this is maxelem - 1.  If not, shrink down.  */
      if (elem < (maxelem - 1))
	vpath = (char **) xrealloc ((char *) vpath,
				    (elem + 1) * sizeof (char *));

      /* Put the nil-pointer terminator on the end of the VPATH list.  */
      vpath[elem] = 0;

      /* Construct the vpath structure and put it into the linked list.  */
      path = (struct vpath *) xmalloc (sizeof (struct vpath));
      path->searchpath = vpath;
      path->maxlen = maxvpath;
      path->next = vpaths;
      vpaths = path;

      /* Set up the members.  */
      path->pattern = pattern;
      path->percent = percent;
      path->patlen = strlen (pattern);
    }
  else
    {
      /* There were no entries, so free whatever space we allocated.  */
      free ((char *) vpath);
      if (pattern != 0)
	free (pattern);
    }
}

/* Search the VPATH list whose pattern matches *FILE for a directory
   where the name pointed to by FILE exists.  If it is found, we set *FILE to
   the newly malloc'd name of the existing file, *MTIME_PTR (if MTIME_PTR is
   not NULL) to its modtime (or zero if no stat call was done), and return 1.
   Otherwise we return 0.  */

int
vpath_search (file, mtime_ptr)
     char **file;
     time_t *mtime_ptr;
{
  register struct vpath *v;

  /* If there are no VPATH entries or FILENAME starts at the root,
     there is nothing we can do.  */

  if (**file == '/'
#ifdef WIN32
      || **file == '\\'
      || (*file)[1] == ':'
#endif
      || (vpaths == 0 && general_vpath == 0))
    return 0;

  for (v = vpaths; v != 0; v = v->next)
    if (pattern_matches (v->pattern, v->percent, *file))
      if (selective_vpath_search (v, file, mtime_ptr))
	return 1;

  if (general_vpath != 0
      && selective_vpath_search (general_vpath, file, mtime_ptr))
    return 1;

  return 0;
}


/* Search the given VPATH list for a directory where the name pointed
   to by FILE exists.  If it is found, we set *FILE to the newly malloc'd
   name of the existing file, *MTIME_PTR (if MTIME_PTR is not NULL) to
   its modtime (or zero if no stat call was done), and we return 1.
   Otherwise we return 0.  */

static int
selective_vpath_search (path, file, mtime_ptr)
     struct vpath *path;
     char **file;
     time_t *mtime_ptr;
{
  int not_target;
  char *name, *n;
  char *filename;
  register char **vpath = path->searchpath;
  unsigned int maxvpath = path->maxlen;
  register unsigned int i;
  unsigned int flen, vlen, name_dplen;
  int exists = 0;

  /* Find out if *FILE is a target.
     If and only if it is NOT a target, we will accept prospective
     files that don't exist but are mentioned in a makefile.  */
  {
    struct file *f = lookup_file (*file);
    not_target = f == 0 || !f->is_target;
  }

  flen = strlen (*file);

  /* Split *FILE into a directory prefix and a name-within-directory.
     NAME_DPLEN gets the length of the prefix; FILENAME gets the
     pointer to the name-within-directory and FLEN is its length.  */

  n = rindex (*file, '/');
#ifdef WIN32
  if (!n)
    n = rindex(*file,, '\\');
#endif
  name_dplen = n != 0 ? n - *file : 0;
  filename = name_dplen > 0 ? n + 1 : *file;
  if (name_dplen > 0)
    flen -= name_dplen + 1;

  /* Allocate enough space for the biggest VPATH entry,
     a slash, the directory prefix that came with *FILE,
     another slash (although this one may not always be
     necessary), the filename, and a null terminator.  */
  name = (char *) xmalloc (maxvpath + 1 + name_dplen + 1 + flen + 1);

  /* Try each VPATH entry.  */
  for (i = 0; vpath[i] != 0; ++i)
    {
      int exists_in_cache = 0;

      n = name;

      /* Put the next VPATH entry into NAME at N and increment N past it.  */
      vlen = strlen (vpath[i]);
      bcopy (vpath[i], n, vlen);
      n += vlen;

      /* Add the directory prefix already in *FILE.  */
      if (name_dplen > 0)
	{
	  *n++ = '/';
	  bcopy (*file, n, name_dplen);
	  n += name_dplen;
	}

      /* Now add the name-within-directory at the end of NAME.  */
      if (n != name && n[-1] != '/')
	{
	  *n = '/';
	  bcopy (filename, n + 1, flen + 1);
	}
      else
	bcopy (filename, n, flen + 1);

      /* Check if the file is mentioned in a makefile.  If *FILE is not
	 a target, that is enough for us to decide this file exists.
	 If *FILE is a target, then the file must be mentioned in the
	 makefile also as a target to be chosen.

	 The restriction that *FILE must not be a target for a
	 makefile-mentioned file to be chosen was added by an
	 inadequately commented change in July 1990; I am not sure off
	 hand what problem it fixes.

	 In December 1993 I loosened of this restriction to allow a file
	 to be chosen if it is mentioned as a target in a makefile.  This
	 seem logical.  */
      {
	struct file *f = lookup_file (name);
	if (f != 0)
	  exists = not_target || f->is_target;
      }

      if (!exists)
	{
	  /* That file wasn't mentioned in the makefile.
	     See if it actually exists.  */

	  /* Clobber a null into the name at the last slash.
	     Now NAME is the name of the directory to look in.  */
	  *n = '\0';

	  /* We know the directory is in the hash table now because either
	     construct_vpath_list or the code just above put it there.
	     Does the file we seek exist in it?  */
	  exists_in_cache = exists = dir_file_exists_p (name, filename);
	}

      if (exists)
	{
	  /* The file is in the directory cache.
	     Now check that it actually exists in the filesystem.
	     The cache may be out of date.  When vpath thinks a file
	     exists, but stat fails for it, confusion results in the
	     higher levels.  */

	  struct stat st;

	  /* Put the slash back in NAME.  */
	  *n = '/';

	  if (!exists_in_cache	/* Makefile-mentioned file need not exist.  */
	      || stat (name, &st) == 0) /* Does it really exist?  */
	    {
	      /* We have found a file.
		 Store the name we found into *FILE for the caller.  */

	      *file = savestring (name, (n + 1 - name) + flen);

	      if (mtime_ptr != 0)
		/* Store the modtime into *MTIME_PTR for the caller.
		   If we have had no need to stat the file here,
		   we record a zero modtime to indicate this.  */
		*mtime_ptr = exists_in_cache ? st.st_mtime : (time_t) 0;

	      free (name);
	      return 1;
	    }
	  else
	    exists = 0;
	}
    }

  free (name);
  return 0;
}

/* Print the data base of VPATH search paths.  */

void
print_vpath_data_base ()
{
  register unsigned int nvpaths;
  register struct vpath *v;

  puts ("\n# VPATH Search Paths\n");

  nvpaths = 0;
  for (v = vpaths; v != 0; v = v->next)
    {
      register unsigned int i;

      ++nvpaths;

      printf ("vpath %s ", v->pattern);

      for (i = 0; v->searchpath[i] != 0; ++i)
	printf ("%s%c", v->searchpath[i],
		v->searchpath[i + 1] == 0 ? '\n' : PATH_SEPARATOR_CHAR);
    }

  if (vpaths == 0)
    puts ("# No `vpath' search paths.");
  else
    printf ("\n# %u `vpath' search paths.\n", nvpaths);

  if (general_vpath == 0)
    puts ("\n# No general (`VPATH' variable) search path.");
  else
    {
      register char **path = general_vpath->searchpath;
      register unsigned int i;

      fputs ("\n# General (`VPATH' variable) search path:\n# ", stdout);

      for (i = 0; path[i] != 0; ++i)
	printf ("%s%c", path[i],
		path[i + 1] == 0 ? '\n' : PATH_SEPARATOR_CHAR);
    }
}



  /*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         version.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

char *version_string = "3.75";

/*
  Local variables:
  version-control: never
  End:
 */



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         ar.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/
/* Interface to `ar' archives for GNU Make.
Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
#undef stderr
#define stderr stdout

#ifndef	NO_ARCHIVES

/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
/* #include "dep.h"  <- modification by J.Ruthruff, 7/20 */
#include <fnmatch.h>
#undef stderr
#define stderr stdout

/* Defined in arscan.c.  */
extern long int ar_scan PARAMS ((char *archive, long int (*function) (), long int arg));
extern int ar_member_touch PARAMS ((char *arname, char *memname));
extern int ar_name_equal PARAMS ((char *name, char *mem, int truncated));


/* Return nonzero if NAME is an archive-member reference, zero if not.
   An archive-member reference is a name like `lib(member)'.
   If a name like `lib((entry))' is used, a fatal error is signaled at
   the attempt to use this unsupported feature.  */

int
ar_name (name)
     char *name;
{
  char *p = index (name, '('), *end = name + strlen (name) - 1;

  if (p == 0 || p == name || *end != ')')
    return 0;

  if (p[1] == '(' && end[-1] == ')')
    fatal ("attempt to use unsupported feature: `%s'", name);

  return 1;
}


/* Parse the archive-member reference NAME into the archive and member names.
   Put the malloc'd archive name in *ARNAME_P if ARNAME_P is non-nil;
   put the malloc'd member name in *MEMNAME_P if MEMNAME_P is non-nil.  */

void
ar_parse_name (name, arname_p, memname_p)
     char *name, **arname_p, **memname_p;
{
  char *p = index (name, '('), *end = name + strlen (name) - 1;

  if (arname_p != 0)
    *arname_p = savestring (name, p - name);

  if (memname_p != 0)
    *memname_p = savestring (p + 1, end - (p + 1));
}

#ifdef VMS
#include <lbrdef.h>
#include <mhddef.h>
#include <credef.h>
#include <descrip.h>
#include <ctype.h>
#undef stderr
#define stderr stdout
#if __DECC
#include <lbr$routines.h>
#undef stderr
#define stderr stdout
#endif

#define uppercasify(str) {char *str1; for (str1 = str; *str1; str1++) *str1 = _toupper(*str1);}

#define LBR$_KEYNOTFND 2527330 /* This isn't in any .h file anywhere so I got it from a MACRO library */

time_t
ar_member_date (name)
     char *name;
{
  static char *arname;
  static char *memname;
  char *p,*q;
  long int val;
  static struct {
    struct mhddef mhd;
    struct credef cre;
    char garbage[256];
  } buf;
  int func=LBR$C_READ,
      type=LBR$C_TYP_OBJ,
      rfa[2],
      lidx,
      status;
  $DESCRIPTOR(bufdesc,(char *)&buf);
  $DESCRIPTOR(libdesc,arname);
  $DESCRIPTOR(moddesc,memname);

  /* This "file" is an archive member.  */
  p = index (name, '(');
  arname = savestring (name, p - name);
  val = strlen (p) - 2;
/*
  if (val > 15)
    val = 15;
*/
  memname = savestring (p + 1, val);
#ifdef OLDWAY
  p = rindex (memname, ')');
  if (p != 0) {
      q = rindex(p,'.');
      if (q)
	*q = '\0'; /* to get rid of extension */
  }
#endif

  q = rindex(memname,'.');
  if (q)
    *q = '\0'; /* to get rid of extension */

  uppercasify(memname);

  /* Make sure we know the modtime of the archive itself because
     we are likely to be called just before commands to remake a
     member are run, and they will change the archive itself.  */
  (void) f_mtime (enter_file (arname));

  libdesc.dsc$a_pointer = arname;
  libdesc.dsc$w_length = strlen(arname);
  moddesc.dsc$a_pointer = memname;
  moddesc.dsc$w_length = strlen(memname);

  if (!((status = lbr$ini_control(&lidx,&func,&type,0)) & 1)) {
    printf("Error in lbr$ini_control, %d\n",status);
    return(-1);
  }

  if (!((status = lbr$open(&lidx,&libdesc,0,0,0,0,0)) & 1)) {
    printf("Error opening library %s to lookup member %s, %d\n",arname, memname ,status);
    return(-1);
  }

  if (!((status = lbr$lookup_key(&lidx,&moddesc,rfa)) & 1)) {
      if (status != LBR$_KEYNOTFND)
	printf("Error looking up module %s in library %s, %d\n",memname, arname ,status);
      lbr$close(&lidx);
      return(-1);
  }

  if (!((status = lbr$set_module(&lidx,rfa,&bufdesc,&bufdesc,0)) & 1)) {
    printf("Error getting module info, %d\n",status);
    lbr$close(&lidx);
    return(-1);
  }

  lbr$close(&lidx);

  val = SHELL$FIX_TIME(&buf.mhd.mhd$l_datim);

  free (arname);
  free (memname);
  return (val <= 0 ? (time_t) -1 : (time_t) val);
}

#else

static long int ar_member_date_1 PARAMS ((int desc, char *mem, int truncated, long int hdrpos,
	long int datapos, long int size, long int date, int uid, int gid, int mode, char *name));

/* Return the modtime of NAME.  */

time_t
ar_member_date (name)
     char *name;
{
  char *arname;
  int arname_used = 0;
  char *memname;
  long int val;

  ar_parse_name (name, &arname, &memname);

  /* Make sure we know the modtime of the archive itself because we are
     likely to be called just before commands to remake a member are run,
     and they will change the archive itself.

     But we must be careful not to enter_file the archive itself if it does
     not exist, because pattern_search assumes that files found in the data
     base exist or can be made.  */
  {
    struct file *arfile;
    arfile = lookup_file (arname);
    if (arfile == 0 && file_exists_p (arname))
      {
	arfile = enter_file (arname);
	arname_used = 1;
      }

    if (arfile != 0)
      (void) f_mtime (arfile, 0);
  }

  val = ar_scan (arname, ar_member_date_1, (long int) memname);

  if (!arname_used)
    free (arname);
  free (memname);

  return (val <= 0 ? (time_t) -1 : (time_t) val);
}

/* This function is called by `ar_scan' to find which member to look at.  */

/* ARGSUSED */
static long int
ar_member_date_1 (desc, mem, truncated,
		  hdrpos, datapos, size, date, uid, gid, mode, name)
     int desc;
     char *mem;
     int truncated;
     long int hdrpos, datapos, size, date;
     int uid, gid, mode;
     char *name;
{
  return ar_name_equal (name, mem, truncated) ? date : 0;
}
#endif /* !VMS */

/* Set the archive-member NAME's modtime to now.  */

#ifdef VMS
int
ar_touch (name)
     char *name;
{
  error ("touch archive member is not available on VMS");
  return -1;
}
#else
int
ar_touch (name)
     char *name;
{
  char *arname, *memname;
  int arname_used = 0;
  register int val;

  ar_parse_name (name, &arname, &memname);

  /* Make sure we know the modtime of the archive itself before we
     touch the member, since this will change the archive itself.  */
  {
    struct file *arfile;
    arfile = lookup_file (arname);
    if (arfile == 0)
      {
	arfile = enter_file (arname);
	arname_used = 1;
      }

    (void) f_mtime (arfile, 0);
  }

  val = 1;
  switch (ar_member_touch (arname, memname))
    {
    case -1:
      error ("touch: Archive `%s' does not exist", arname);
      break;
    case -2:
      error ("touch: `%s' is not a valid archive", arname);
      break;
    case -3:
      perror_with_name ("touch: ", arname);
      break;
    case 1:
      error ("touch: Member `%s' does not exist in `%s'", memname, arname);
      break;
    case 0:
      val = 0;
      break;
    default:
      error ("touch: Bad return code from ar_member_touch on `%s'", name);
    }

  if (!arname_used)
    free (arname);
  free (memname);

  return val;
}
#endif /* !VMS */

/* State of an `ar_glob' run, passed to `ar_glob_match'.  */

struct ar_glob_state
  {
    char *arname;
    char *pattern;
    unsigned int size;
    struct nameseq *chain;
    unsigned int n;
  };

/* This function is called by `ar_scan' to match one archive
   element against the pattern in STATE.  */

static long int
ar_glob_match (desc, mem, truncated,
	       hdrpos, datapos, size, date, uid, gid, mode,
	       state)
     int desc;
     char *mem;
     int truncated;
     long int hdrpos, datapos, size, date;
     int uid, gid, mode;
     struct ar_glob_state *state;
{
  if (fnmatch (state->pattern, mem, FNM_PATHNAME|FNM_PERIOD) == 0)
    {
      /* We have a match.  Add it to the chain.  */
      struct nameseq *new = (struct nameseq *) xmalloc (state->size);
      new->name = concat (state->arname, mem, ")");
      new->next = state->chain;
      state->chain = new;
      ++state->n;
    }

  return 0L;
}

/* Alphabetic sorting function for `qsort'.  */

static int
ar_glob_alphacompare (a, b)
     char **a, **b;
{
  return strcmp (*a, *b);
}

/* Return nonzero if PATTERN contains any metacharacters.
   Metacharacters can be quoted with backslashes if QUOTE is nonzero.  */
static int
glob_pattern_p (pattern, quote)
     const char *pattern;
     const int quote;
{
  register const char *p;
  int open = 0;

  for (p = pattern; *p != '\0'; ++p)
    switch (*p)
      {
      case '?': case_coverage = case_coverage;
      case '*':
	return 1;

      case '\\':
	if (quote)
	  ++p;
	break;

      case '[':
	open = 1;
	break;

      case ']':
	if (open)
	  return 1;
	break;
      }

  return 0;
}

/* Glob for MEMBER_PATTERN in archive ARNAME.
   Return a malloc'd chain of matching elements (or nil if none).  */

struct nameseq *
ar_glob (arname, member_pattern, size)
     char *arname, *member_pattern;
     unsigned int size;
{
  struct ar_glob_state state;
  char **names;
  struct nameseq *n;
  unsigned int i;

  if (! glob_pattern_p (member_pattern, 1))
    return 0;

  /* Scan the archive for matches.
     ar_glob_match will accumulate them in STATE.chain.  */
  i = strlen (arname);
  state.arname = (char *) alloca (i + 2);
  bcopy (arname, state.arname, i);
  state.arname[i] = '(';
  state.arname[i + 1] = '\0';
  state.pattern = member_pattern;
  state.size = size;
  state.chain = 0;
  state.n = 0;
  (void) ar_scan (arname, ar_glob_match, (long int) &state);

  if (state.chain == 0)
    return 0;

  /* Now put the names into a vector for sorting.  */
  names = (char **) alloca (state.n * sizeof (char *));
  i = 0;
  for (n = state.chain; n != 0; n = n->next)
    names[i++] = n->name;

  /* Sort them alphabetically.  */
  qsort ((char *) names, i, sizeof (*names), ar_glob_alphacompare);

  /* Put them back into the chain in the sorted order.  */
  i = 0;
  for (n = state.chain; n != 0; n = n->next)
    n->name = names[i++];

  return state.chain;
}

#endif	/* Not NO_ARCHIVES.  */



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         arscan.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Library function for scanning an archive file.
   Copyright (C) 1987, 89, 91, 92, 93, 94, 95 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
#undef stderr
#define stderr stdout

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#undef stderr
#define stderr stdout
#else
#include <sys/file.h>
#undef stderr
#define stderr stdout
#endif

#ifndef	NO_ARCHIVES

/* SCO Unix's compiler defines both of these.  */
#ifdef	M_UNIX
#undef	M_XENIX
#endif

/* On the sun386i and in System V rel 3, ar.h defines two different archive
   formats depending upon whether you have defined PORTAR (normal) or PORT5AR
   (System V Release 1).  There is no default, one or the other must be defined
   to have a nonzero value.  */

#if (!defined (PORTAR) || PORTAR == 0) && (!defined (PORT5AR) || PORT5AR == 0)
#undef	PORTAR
#ifdef M_XENIX
/* According to Jim Sievert <jas1@rsvl.unisys.com>, for SCO XENIX defining
   PORTAR to 1 gets the wrong archive format, and defining it to 0 gets the
   right one.  */
#define PORTAR 0
#else
#define PORTAR 1
#endif
#endif

#include <ar.h>
#undef stderr
#define stderr stdout

/* Cray's <ar.h> apparently defines this.  */
#ifndef	AR_HDR_SIZE
#define	AR_HDR_SIZE	(sizeof (struct ar_hdr))
#endif

/* Takes three arguments ARCHIVE, FUNCTION and ARG.

   Open the archive named ARCHIVE, find its members one by one,
   and for each one call FUNCTION with the following arguments:
     archive file descriptor for reading the data,
     member name,
     member name might be truncated flag,
     member header position in file,
     member data position in file,
     member data size,
     member date,
     member uid,
     member gid,
     member protection mode,
     ARG.

   The descriptor is poised to read the data of the member
   when FUNCTION is called.  It does not matter how much
   data FUNCTION reads.

   If FUNCTION returns nonzero, we immediately return
   what FUNCTION returned.

   Returns -1 if archive does not exist,
   Returns -2 if archive has invalid format.
   Returns 0 if have scanned successfully.  */

long int
ar_scan (archive, function, arg)
     char *archive;
     long int (*function) ();
     long int arg;
{
#ifdef AIAMAG
  FL_HDR fl_header;
#else
  int long_name = 0;
#endif
  char *namemap = 0;
  register int desc = open (archive, O_RDONLY, 0);
  if (desc < 0)
    return -1;
#ifdef SARMAG
  {
    char buf[SARMAG];
    register int nread = read (desc, buf, SARMAG);
    if (nread != SARMAG || bcmp (buf, ARMAG, SARMAG))
      {
	(void) close (desc);
	return -2;
      }
  }
#else
#ifdef AIAMAG
  {
    register int nread = read (desc, (char *) &fl_header, FL_HSZ);
    if (nread != FL_HSZ || bcmp (fl_header.fl_magic, AIAMAG, SAIAMAG))
      {
	(void) close (desc);
	return -2;
      }
  }
#else
  {
#ifndef M_XENIX
    int buf;
#else
    unsigned short int buf;
#endif
    register int nread = read(desc, &buf, sizeof (buf));
    if (nread != sizeof (buf) || buf != ARMAG)
      {
	(void) close (desc);
	return -2;
      }
  }
#endif
#endif

  /* Now find the members one by one.  */
  {
#ifdef SARMAG
    register long int member_offset = SARMAG;
#else
#ifdef AIAMAG
    long int member_offset;
    long int last_member_offset;

    sscanf (fl_header.fl_fstmoff, "%12ld", &member_offset);
    sscanf (fl_header.fl_lstmoff, "%12ld", &last_member_offset);

    if (member_offset == 0)
      {
	/* Empty archive.  */
	close (desc);
	return 0;
      }
#else
#ifndef	M_XENIX
    register long int member_offset = sizeof (int);
#else	/* Xenix.  */
    register long int member_offset = sizeof (unsigned short int);
#endif	/* Not Xenix.  */
#endif
#endif

    while (1)
      {
	register int nread;
	struct ar_hdr member_header;
#ifdef AIAMAG
	char name[256];
	int name_len;
	long int dateval;
	int uidval, gidval;
	long int data_offset;
#else
	char namebuf[sizeof member_header.ar_name + 1];
	char *name;
	int is_namemap;		/* Nonzero if this entry maps long names.  */
#endif
	long int eltsize;
	int eltmode;
	long int fnval;

	if (lseek (desc, member_offset, 0) < 0)
	  {
	    (void) close (desc);
	    return -2;
	  }

#ifdef AIAMAG
#define	AR_MEMHDR	(AR_HDR_SIZE - sizeof (member_header._ar_name))
	nread = read (desc, (char *) &member_header, AR_MEMHDR);

	if (nread != AR_MEMHDR)
	  {
	    (void) close (desc);
	    return -2;
	  }

	sscanf (member_header.ar_namlen, "%4d", &name_len);
	nread = read (desc, name, name_len);

	if (nread != name_len)
	  {
	    (void) close (desc);
	    return -2;
	  }

	name[name_len] = 0;

	sscanf (member_header.ar_date, "%12ld", &dateval);
	sscanf (member_header.ar_uid, "%12d", &uidval);
	sscanf (member_header.ar_gid, "%12d", &gidval);
	sscanf (member_header.ar_mode, "%12o", &eltmode);
	sscanf (member_header.ar_size, "%12ld", &eltsize);

	if ((data_offset = member_offset + AR_MEMHDR + name_len + 2) % 2)
	    ++data_offset;

	fnval =
	  (*function) (desc, name, 0,
		       member_offset, data_offset, eltsize,
		       dateval, uidval, gidval,
		       eltmode, arg);

#else	/* Not AIAMAG.  */
	nread = read (desc, (char *) &member_header, AR_HDR_SIZE);
	if (nread == 0)
	  /* No data left means end of file; that is OK.  */
	  break;

	if (nread != AR_HDR_SIZE
#ifdef ARFMAG
	    || bcmp (member_header.ar_fmag, ARFMAG, 2)
#endif
	    )
	  {
	    (void) close (desc);
	    return -2;
	  }

	name = namebuf;
	bcopy (member_header.ar_name, name, sizeof member_header.ar_name);
	{
	  register char *p = name + sizeof member_header.ar_name;
	  do
	    *p = '\0';
	  while (p > name && *--p == ' ');

#ifndef AIAMAG
	  /* If the member name is "//" or "ARFILENAMES/" this may be
	     a list of file name mappings.  The maximum file name
 	     length supported by the standard archive format is 14
 	     characters.  This member will actually always be the
 	     first or second entry in the archive, but we don't check
 	     that.  */
 	  is_namemap = (!strcmp (name, "//")
			|| !strcmp (name, "ARFILENAMES/"));
#endif	/* Not AIAMAG. */
	  /* On some systems, there is a slash after each member name.  */
	  if (*p == '/')
	    *p = '\0';

#ifndef AIAMAG
 	  /* If the member name starts with a space or a slash, this
 	     is an index into the file name mappings (used by GNU ar).
 	     Otherwise if the member name looks like #1/NUMBER the
 	     real member name appears in the element data (used by
 	     4.4BSD).  */
 	  if (! is_namemap
 	      && (name[0] == ' ' || name[0] == '/')
 	      && namemap != 0)
	    {
	      name = namemap + atoi (name + 1);
	      long_name = 1;
	    }
 	  else if (name[0] == '#'
 		   && name[1] == '1'
 		   && name[2] == '/')
 	    {
 	      int namesize = atoi (name + 3);

 	      name = (char *) alloca (namesize + 1);
 	      nread = read (desc, name, namesize);
 	      if (nread != namesize)
 		{
 		  close (desc);
 		  return -2;
 		}
 	      name[namesize] = '\0';

	      long_name = 1;
 	    }
#endif /* Not AIAMAG. */
	}

#ifndef	M_XENIX
	sscanf (member_header.ar_mode, "%o", &eltmode);
	eltsize = atol (member_header.ar_size);
#else	/* Xenix.  */
	eltmode = (unsigned short int) member_header.ar_mode;
	eltsize = member_header.ar_size;
#endif	/* Not Xenix.  */

	fnval =
	  (*function) (desc, name, ! long_name, member_offset,
		       member_offset + AR_HDR_SIZE, eltsize,
#ifndef	M_XENIX
		       atol (member_header.ar_date),
		       atoi (member_header.ar_uid),
		       atoi (member_header.ar_gid),
#else	/* Xenix.  */
		       member_header.ar_date,
		       member_header.ar_uid,
		       member_header.ar_gid,
#endif	/* Not Xenix.  */
		       eltmode, arg);

#endif  /* AIAMAG.  */

	if (fnval)
	  {
	    (void) close (desc);
	    return fnval;
	  }

#ifdef AIAMAG
	if (member_offset == last_member_offset)
	  /* End of the chain.  */
	  break;

	sscanf (member_header.ar_nxtmem, "%12ld", &member_offset);

	if (lseek (desc, member_offset, 0) != member_offset)
	  {
	    (void) close (desc);
	    return -2;
	  }
#else

 	/* If this member maps archive names, we must read it in.  The
 	   name map will always precede any members whose names must
 	   be mapped.  */
	if (is_namemap)
 	  {
 	    char *clear;
 	    char *limit;

 	    namemap = (char *) alloca (eltsize);
 	    nread = read (desc, namemap, eltsize);
 	    if (nread != eltsize)
 	      {
 		(void) close (desc);
 		return -2;
 	      }

 	    /* The names are separated by newlines.  Some formats have
 	       a trailing slash.  Null terminate the strings for
 	       convenience.  */
 	    limit = namemap + eltsize;
 	    for (clear = namemap; clear < limit; clear++)
 	      {
 		if (*clear == '\n')
 		  {
 		    *clear = '\0';
 		    if (clear[-1] == '/')
 		      clear[-1] = '\0';
 		  }
 	      }

	    is_namemap = 0;
 	  }

	member_offset += AR_HDR_SIZE + eltsize;
	if (member_offset % 2 != 0)
	  member_offset++;
#endif
      }
  }

  close (desc);
  return 0;
}

/* Return nonzero iff NAME matches MEM.
   If TRUNCATED is nonzero, MEM may be truncated to
   sizeof (struct ar_hdr.ar_name) - 1.  */

int
ar_name_equal (name, mem, truncated)
     char *name, *mem;
     int truncated;
{
  char *p;

  p = rindex (name, '/');
  if (p != 0)
    name = p + 1;

  /* We no longer use this kludge, since we
     now support long archive member names.  */

#if 0 && !defined (AIAMAG) && !defined (APOLLO)

  {
    /* `reallylongname.o' matches `reallylongnam.o'.
       If member names have a trailing slash, that's `reallylongna.o'.  */

    struct ar_hdr h;
    unsigned int max = sizeof (h.ar_name);
    unsigned int namelen, memlen;

    if (strncmp (name, mem, max - 3))
      return 0;

    namelen = strlen (name);
    memlen = strlen (mem);

    if (namelen > memlen && memlen >= max - 1
	&& name[namelen - 2] == '.' && name[namelen - 1] == 'o'
	&& mem[memlen - 2] == '.' && mem[memlen - 1] == 'o')
      return 1;

    if (namelen != memlen)
      return 0;

    return (namelen < max - 3 || !strcmp (name + max - 3, mem + max - 3));
  }

#else	/* AIX or APOLLO.  */

  if (truncated)
    {
#ifdef AIAMAG
      /* TRUNCATED should never be set on this system.  */
      abort ();
#else
      struct ar_hdr hdr;
      return !strncmp (name, mem,
		       sizeof (hdr.ar_name) -
#if !defined (__hpux) && !defined (cray)
		       1
#else
		       2
#endif /* !__hpux && !cray */
		       );
#endif
    }

  return !strcmp (name, mem);

#endif
}

/* ARGSUSED */
static long int
ar_member_pos (desc, mem, truncated,
	       hdrpos, datapos, size, date, uid, gid, mode, name)
     int desc;
     char *mem;
     int truncated;
     long int hdrpos, datapos, size, date;
     int uid, gid, mode;
     char *name;
{
  if (!ar_name_equal (name, mem, truncated))
    return 0;
  return hdrpos;
}

/* Set date of member MEMNAME in archive ARNAME to current time.
   Returns 0 if successful,
   -1 if file ARNAME does not exist,
   -2 if not a valid archive,
   -3 if other random system call error (including file read-only),
   1 if valid but member MEMNAME does not exist.  */

int
ar_member_touch (arname, memname)
     char *arname, *memname;
{
  register long int pos = ar_scan (arname, ar_member_pos, (long int) memname);
  register int fd;
  struct ar_hdr ar_hdr;
  register int i;
  struct stat statbuf;

  if (pos < 0)
    return (int) pos;
  if (!pos)
    return 1;

  fd = open (arname, O_RDWR, 0666);
  if (fd < 0)
    return -3;
  /* Read in this member's header */
  if (lseek (fd, pos, 0) < 0)
    goto lose;
  if (AR_HDR_SIZE != read (fd, (char *) &ar_hdr, AR_HDR_SIZE))
    goto lose;
  /* Write back the header, thus touching the archive file.  */
  if (lseek (fd, pos, 0) < 0)
    goto lose;
  if (AR_HDR_SIZE != write (fd, (char *) &ar_hdr, AR_HDR_SIZE))
    goto lose;
  /* The file's mtime is the time we we want.  */
#ifdef EINTR
  while (fstat (fd, &statbuf) < 0 && errno == EINTR);
#else
  fstat (fd, &statbuf);
#endif
#if defined(ARFMAG) || defined(AIAMAG)
  /* Advance member's time to that time */
  for (i = 0; i < sizeof ar_hdr.ar_date; i++)
    ar_hdr.ar_date[i] = ' ';
  sprintf (ar_hdr.ar_date, "%ld", (long int) statbuf.st_mtime);
#ifdef AIAMAG
  ar_hdr.ar_date[strlen(ar_hdr.ar_date)] = ' ';
#endif
#else
  ar_hdr.ar_date = statbuf.st_mtime;
#endif
  /* Write back this member's header */
  if (lseek (fd, pos, 0) < 0)
    goto lose;
  if (AR_HDR_SIZE != write (fd, (char *) &ar_hdr, AR_HDR_SIZE))
    goto lose;
  close (fd);
  return 0;

 lose:
  i = errno;
  close (fd);
  errno = i;
  return -3;
}

#ifdef TEST

long int
describe_member (desc, name, truncated,
		 hdrpos, datapos, size, date, uid, gid, mode)
     int desc;
     char *name;
     int truncated;
     long int hdrpos, datapos, size, date;
     int uid, gid, mode;
{
  extern char *ctime ();

  printf ("Member `%s'%s: %ld bytes at %ld (%ld).\n",
	  name, truncated ? " (name might be truncated)" : "",
	  size, hdrpos, datapos);
  printf ("  Date %s", ctime (&date));
  printf ("  uid = %d, gid = %d, mode = 0%o.\n", uid, gid, mode);

  return 0;
}

main (argc, argv)
     int argc;
     char **argv;
{
  ar_scan (argv[1], describe_member);
  return 0;
}

#endif	/* TEST.  */

#endif	/* NO_ARCHIVES.  */



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         signame.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Convert between signal names and numbers.
   Copyright (C) 1990, 1992, 1993, 1995, 1996 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#undef stderr
#define stderr stdout
#endif

#include <stdio.h>
#include <sys/types.h>		/* Some systems need this for <signal.h>.  */
#include <signal.h>
#undef stderr
#define stderr stdout

#ifdef HAVE_STRING_H
#include <string.h>
#undef stderr
#define stderr stdout
#endif

/* Some systems declare `sys_siglist in <unistd.h>; if
   configure defined SYS_SIGLIST_DECLARED, it may expect
   to find the declaration there.  */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#undef stderr
#define stderr stdout
#endif


/* Some systems do not define NSIG in <signal.h>.  */
#ifndef	NSIG
#ifdef	_NSIG
#define	NSIG	_NSIG
#else
#define	NSIG	32
#endif
#endif

#if !__STDC__
#define const
#endif

#include "signame.h"
#undef stderr
#define stderr stdout

#ifndef HAVE_SYS_SIGLIST
/* There is too much variation in Sys V signal numbers and names, so
   we must initialize them at runtime.  */

static const char undoc[] = "unknown signal";

const char *sys_siglist[NSIG];

#else	/* HAVE_SYS_SIGLIST.  */

#ifndef SYS_SIGLIST_DECLARED
extern char *sys_siglist[];
#endif	/* Not SYS_SIGLIST_DECLARED.  */

#endif	/* Not HAVE_SYS_SIGLIST.  */

/* Table of abbreviations for signals.  Note:  A given number can
   appear more than once with different abbreviations.  */
typedef struct
  {
    int number;
    const char *abbrev;
  } num_abbrev;
static num_abbrev sig_table[NSIG*2];
/* Number of elements of sig_table used.  */
static int sig_table_nelts = 0;

/* Enter signal number NUMBER into the tables with ABBREV and NAME.  */

static void
init_sig (number, abbrev, name)
     int number;
     const char *abbrev;
     const char *name;
{
#ifndef HAVE_SYS_SIGLIST
  sys_siglist[number] = name;
#endif
  sig_table[sig_table_nelts].number = number;
  sig_table[sig_table_nelts++].abbrev = abbrev;
}

void
signame_init ()
{
#ifndef HAVE_SYS_SIGLIST
  int i;
  /* Initialize signal names.  */
  for (i = 0; i < NSIG; i++)
    sys_siglist[i] = undoc;
#endif /* !HAVE_SYS_SIGLIST */

  /* Initialize signal names.  */
#if defined (SIGHUP)
  init_sig (SIGHUP, "HUP", "Hangup");
#endif
#if defined (SIGINT)
  init_sig (SIGINT, "INT", "Interrupt");
#endif
#if defined (SIGQUIT)
  init_sig (SIGQUIT, "QUIT", "Quit");
#endif
#if defined (SIGILL)
  init_sig (SIGILL, "ILL", "Illegal Instruction");
#endif
#if defined (SIGTRAP)
  init_sig (SIGTRAP, "TRAP", "Trace/breakpoint trap");
#endif
  /* If SIGIOT == SIGABRT, we want to print it as SIGABRT because
     SIGABRT is in ANSI and POSIX.1 and SIGIOT isn't.  */
#if defined (SIGABRT)
  init_sig (SIGABRT, "ABRT", "Aborted");
#endif
#if defined (SIGIOT)
  init_sig (SIGIOT, "IOT", "IOT trap");
#endif
#if defined (SIGEMT)
  init_sig (SIGEMT, "EMT", "EMT trap");
#endif
#if defined (SIGFPE)
  init_sig (SIGFPE, "FPE", "Floating point exception");
#endif
#if defined (SIGKILL)
  init_sig (SIGKILL, "KILL", "Killed");
#endif
#if defined (SIGBUS)
  init_sig (SIGBUS, "BUS", "Bus error");
#endif
#if defined (SIGSEGV)
  init_sig (SIGSEGV, "SEGV", "Segmentation fault");
#endif
#if defined (SIGSYS)
  init_sig (SIGSYS, "SYS", "Bad system call");
#endif
#if defined (SIGPIPE)
  init_sig (SIGPIPE, "PIPE", "Broken pipe");
#endif
#if defined (SIGALRM)
  init_sig (SIGALRM, "ALRM", "Alarm clock");
#endif
#if defined (SIGTERM)
  init_sig (SIGTERM, "TERM", "Terminated");
#endif
#if defined (SIGUSR1)
  init_sig (SIGUSR1, "USR1", "User defined signal 1");
#endif
#if defined (SIGUSR2)
  init_sig (SIGUSR2, "USR2", "User defined signal 2");
#endif
  /* If SIGCLD == SIGCHLD, we want to print it as SIGCHLD because that
     is what is in POSIX.1.  */
#if defined (SIGCHLD)
  init_sig (SIGCHLD, "CHLD", "Child exited");
#endif
#if defined (SIGCLD)
  init_sig (SIGCLD, "CLD", "Child exited");
#endif
#if defined (SIGPWR)
  init_sig (SIGPWR, "PWR", "Power failure");
#endif
#if defined (SIGTSTP)
  init_sig (SIGTSTP, "TSTP", "Stopped");
#endif
#if defined (SIGTTIN)
  init_sig (SIGTTIN, "TTIN", "Stopped (tty input)");
#endif
#if defined (SIGTTOU)
  init_sig (SIGTTOU, "TTOU", "Stopped (tty output)");
#endif
#if defined (SIGSTOP)
  init_sig (SIGSTOP, "STOP", "Stopped (signal)");
#endif
#if defined (SIGXCPU)
  init_sig (SIGXCPU, "XCPU", "CPU time limit exceeded");
#endif
#if defined (SIGXFSZ)
  init_sig (SIGXFSZ, "XFSZ", "File size limit exceeded");
#endif
#if defined (SIGVTALRM)
  init_sig (SIGVTALRM, "VTALRM", "Virtual timer expired");
#endif
#if defined (SIGPROF)
  init_sig (SIGPROF, "PROF", "Profiling timer expired");
#endif
#if defined (SIGWINCH)
  /* "Window size changed" might be more accurate, but even if that
     is all that it means now, perhaps in the future it will be
     extended to cover other kinds of window changes.  */
  init_sig (SIGWINCH, "WINCH", "Window changed");
#endif
#if defined (SIGCONT)
  init_sig (SIGCONT, "CONT", "Continued");
#endif
#if defined (SIGURG)
  init_sig (SIGURG, "URG", "Urgent I/O condition");
#endif
#if defined (SIGIO)
  /* "I/O pending" has also been suggested.  A disadvantage is
     that signal only happens when the process has
     asked for it, not everytime I/O is pending.  Another disadvantage
     is the confusion from giving it a different name than under Unix.  */
  init_sig (SIGIO, "IO", "I/O possible");
#endif
#if defined (SIGWIND)
  init_sig (SIGWIND, "WIND", "SIGWIND");
#endif
#if defined (SIGPHONE)
  init_sig (SIGPHONE, "PHONE", "SIGPHONE");
#endif
#if defined (SIGPOLL)
  init_sig (SIGPOLL, "POLL", "I/O possible");
#endif
#if defined (SIGLOST)
  init_sig (SIGLOST, "LOST", "Resource lost");
#endif
#if defined (SIGDANGER)
  init_sig (SIGDANGER, "DANGER", "Danger signal");
#endif
#if defined (SIGINFO)
  init_sig (SIGINFO, "INFO", "Information request");
#endif
}

/* Return the abbreviation for signal NUMBER.  */

char *
sig_abbrev (number)
     int number;
{
  int i;

  if (sig_table_nelts == 0)
    signame_init ();

  for (i = 0; i < sig_table_nelts; i++)
    if (sig_table[i].number == number)
      return (char *)sig_table[i].abbrev;
  return NULL;
}

/* Return the signal number for an ABBREV, or -1 if there is no
   signal by that name.  */

int
sig_number (abbrev)
     const char *abbrev;
{
  int i;

  if (sig_table_nelts == 0)
    signame_init ();

  /* Skip over "SIG" if present.  */
  if (abbrev[0] == 'S' && abbrev[1] == 'I' && abbrev[2] == 'G')
    abbrev += 3;

  for (i = 0; i < sig_table_nelts; i++)
    if (abbrev[0] == sig_table[i].abbrev[0]
	&& strcmp (abbrev, sig_table[i].abbrev) == 0)
      return sig_table[i].number;
  return -1;
}

#ifndef HAVE_PSIGNAL
/* Print to standard error the name of SIGNAL, preceded by MESSAGE and
   a colon, and followed by a newline.  */

void
psignal (signal, message)
     int signal;
     const char *message;
{
  if (signal <= 0 || signal >= NSIG)
    fprintf (stderr, "%s: unknown signal", message);
  else
    fprintf (stderr, "%s: %s\n", message, sys_siglist[signal]);
}
#endif

#ifndef HAVE_STRSIGNAL
/* Return the string associated with the signal number.  */

char *
strsignal (signal)
     int signal;
{
  static char buf[] = "Signal 12345678901234567890";

  if (signal > 0 || signal < NSIG)
    return (char *) sys_siglist[signal];

  sprintf (buf, "Signal %d", signal);
  return buf;
}
#endif



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         remote-stub.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Template for the remote job exportation interface to GNU Make.
Copyright (C) 1988, 1989, 1992, 1993, 1996 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Make is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Make; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "make.h"
/* #include "filedef.h"  <- modification by J.Ruthruff, 7/20 */
#include "job.h"
/* #include "commands.h"  <- modification by J.Ruthruff, 7/20 */
#undef stderr
#define stderr stdout

char *remote_description = 0;

/* Call once at startup even if no commands are run.  */

void
remote_setup ()
{
}

/* Called before exit.  */

void
remote_cleanup ()
{
}

/* Return nonzero if the next job should be done remotely.  */

int
start_remote_job_p ()
{
  return 0;
}

/* Start a remote job running the command in ARGV,
   with environment from ENVP.  It gets standard input from STDIN_FD.  On
   failure, return nonzero.  On success, return zero, and set *USED_STDIN
   to nonzero if it will actually use STDIN_FD, zero if not, set *ID_PTR to
   a unique identification, and set *IS_REMOTE to zero if the job is local,
   nonzero if it is remote (meaning *ID_PTR is a process ID).  */

int
start_remote_job (argv, envp, stdin_fd, is_remote, id_ptr, used_stdin)
     char **argv, **envp;
     int stdin_fd;
     int *is_remote;
     int *id_ptr;
     int *used_stdin;
{
  return -1;
}

/* Get the status of a dead remote child.  Block waiting for one to die
   if BLOCK is nonzero.  Set *EXIT_CODE_PTR to the exit status, *SIGNAL_PTR
   to the termination signal or zero if it exited normally, and *COREDUMP_PTR
   nonzero if it dumped core.  Return the ID of the child that died,
   0 if we would have to block and !BLOCK, or < 0 if there were none.  */

int
remote_status (exit_code_ptr, signal_ptr, coredump_ptr, block)
     int *exit_code_ptr, *signal_ptr, *coredump_ptr;
     int block;
{
  errno = ECHILD;
  return -1;
}

/* Block asynchronous notification of remote child death.
   If this notification is done by raising the child termination
   signal, do not block that signal.  */
void
block_remote_children ()
{
  return;
}

/* Restore asynchronous notification of remote child death.
   If this is done by raising the child termination signal,
   do not unblock that signal.  */
void
unblock_remote_children ()
{
  return;
}

/* Send signal SIG to child ID.  Return 0 if successful, -1 if not.  */
int
remote_kill (id, sig)
     int id;
     int sig;
{
  return -1;
}



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         getopt.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* Getopt for GNU.
   NOTE: getopt is now part of the C library, so if you don't know what
   "Keep this file name-space clean" means, talk to roland@gnu.ai.mit.edu
   before changing it!

   Copyright (C) 1987, 88, 89, 90, 91, 92, 93, 94, 95, 1996
   	Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
   USA.  */

/* This tells Alpha OSF/1 not to define a getopt prototype in <stdio.h>.
   Ditto for AIX 3.2 and <stdlib.h>.  */
#ifndef _NO_PROTO
#define _NO_PROTO
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#undef stderr
#define stderr stdout
#endif

#if !defined (__STDC__) || !__STDC__
/* This is a separate conditional since some stdc systems
   reject `defined (const)'.  */
#ifndef const
#define const
#endif
#endif

#include <stdio.h>
#undef stderr
#define stderr stdout

/* Comment out all this code if we are using the GNU C Library, and are not
   actually compiling the library itself.  This code is part of the GNU C
   Library, but also included in many other GNU distributions.  Compiling
   and linking in this code is a waste when using the GNU C library
   (especially if it is a shared library).  Rather than having every GNU
   program understand `configure --with-gnu-libc' and omit the object files,
   it is simpler to just do this in the source for each such file.  */

#if defined (_LIBC) || !defined (__GNU_LIBRARY__)


/* This needs to come after some library #include
   to get __GNU_LIBRARY__ defined.  */
#ifdef	__GNU_LIBRARY__
/* Don't include stdlib.h for non-GNU C libraries because some of them
   contain conflicting prototypes for getopt.  */
#include <stdlib.h>
#include <unistd.h>
#undef stderr
#define stderr stdout
#endif	/* GNU C library.  */

#ifdef VMS
#include <unixlib.h>
#undef stderr
#define stderr stdout
#if HAVE_STRING_H - 0
#include <string.h>
#undef stderr
#define stderr stdout
#endif
#endif

#if defined (_WIN32) && !defined (__CYGWIN32__)
/* It's not Unix, really.  See?  Capital letters.  */
#include <windows.h>
#undef stderr
#define stderr stdout
#define getpid() GetCurrentProcessId()
#endif

#ifndef _
/* This is for other GNU distributions with internationalized messages.
   When compiling libc, the _ macro is predefined.  */
#ifdef HAVE_LIBINTL_H
# include <libintl.h>
#undef stderr
#define stderr stdout
# define _(msgid)	gettext (msgid)
#else
# define _(msgid)	(msgid)
#endif
#endif

/* This version of `getopt' appears to the caller like standard Unix `getopt'
   but it behaves differently for the user, since it allows the user
   to intersperse the options with the other arguments.

   As `getopt' works, it permutes the elements of ARGV so that,
   when it is done, all the options precede everything else.  Thus
   all application programs are extended to handle flexible argument order.

   Setting the environment variable POSIXLY_CORRECT disables permutation.
   Then the behavior is completely standard.

   GNU application programs can use a third alternative mode in which
   they can distinguish the relative order of options and other arguments.  */

#include "getopt.h"
#undef stderr
#define stderr stdout

/* For communication from `getopt' to the caller.
   When `getopt' finds an option that takes an argument,
   the argument value is returned here.
   Also, when `ordering' is RETURN_IN_ORDER,
   each non-option ARGV-element is returned here.  */

char *optarg = NULL;

/* Index in ARGV of the next element to be scanned.
   This is used for communication to and from the caller
   and for communication between successive calls to `getopt'.

   On entry to `getopt', zero means this is the first call; initialize.

   When `getopt' returns EOF, this is the index of the first of the
   non-option elements that the caller should itself scan.

   Otherwise, `optind' communicates from one call to the next
   how much of ARGV has been scanned so far.  */

/* XXX 1003.2 says this must be 1 before any call.  */
int optind = 0;

/* The next char to be scanned in the option-element
   in which the last option character we returned was found.
   This allows us to pick up the scan where we left off.

   If this is zero, or a null string, it means resume the scan
   by advancing to the next ARGV-element.  */

static char *nextchar;

/* Callers store zero here to inhibit the error message
   for unrecognized options.  */

int opterr = 1;

/* Set to an option character which was unrecognized.
   This must be initialized on some systems to avoid linking in the
   system's own getopt implementation.  */

int optopt = '?';

/* Describe how to deal with options that follow non-option ARGV-elements.

   If the caller did not specify anything,
   the default is REQUIRE_ORDER if the environment variable
   POSIXLY_CORRECT is defined, PERMUTE otherwise.

   REQUIRE_ORDER means don't recognize them as options;
   stop option processing when the first non-option is seen.
   This is what Unix does.
   This mode of operation is selected by either setting the environment
   variable POSIXLY_CORRECT, or using `+' as the first character
   of the list of option characters.

   PERMUTE is the default.  We permute the contents of ARGV as we scan,
   so that eventually all the non-options are at the end.  This allows options
   to be given in any order, even with programs that were not written to
   expect this.

   RETURN_IN_ORDER is an option available to programs that were written
   to expect options and other ARGV-elements in any order and that care about
   the ordering of the two.  We describe each non-option ARGV-element
   as if it were the argument of an option with character code 1.
   Using `-' as the first character of the list of option characters
   selects this mode of operation.

   The special argument `--' forces an end of option-scanning regardless
   of the value of `ordering'.  In the case of RETURN_IN_ORDER, only
   `--' can cause `getopt' to return EOF with `optind' != ARGC.  */

static enum
{
  REQUIRE_ORDER, PERMUTE, RETURN_IN_ORDER
} ordering;

/* Value of POSIXLY_CORRECT environment variable.  */
static char *posixly_correct;

#ifdef	__GNU_LIBRARY__
/* We want to avoid inclusion of string.h with non-GNU libraries
   because there are many ways it can cause trouble.
   On some systems, it contains special magic macros that don't work
   in GCC.  */
#include <string.h>
#undef stderr
#define stderr stdout
#define	my_index	strchr
#else

/* Avoid depending on library functions or files
   whose names are inconsistent.  */

char *getenv ();

static char *
my_index (str, chr)
     const char *str;
     int chr;
{
  while (*str)
    {
      if (*str == chr)
	return (char *) str;
      str++;
    }
  return 0;
}

/* If using GCC, we can safely declare strlen this way.
   If not using GCC, it is ok not to declare it.  */
#ifdef __GNUC__
/* Note that Motorola Delta 68k R3V7 comes with GCC but not stddef.h.
   That was relevant to code that was here before.  */
#if !defined (__STDC__) || !__STDC__
/* gcc with -traditional declares the built-in strlen to return int,
   and has done so at least since version 2.4.5. -- rms.  */
extern int strlen (const char *);
#endif /* not __STDC__ */
#endif /* __GNUC__ */

#endif /* not __GNU_LIBRARY__ */

/* Handle permutation of arguments.  */

/* Describe the part of ARGV that contains non-options that have
   been skipped.  `first_nonopt' is the index in ARGV of the first of them;
   `last_nonopt' is the index after the last of them.  */

static int first_nonopt;
static int last_nonopt;

/* Bash 2.0 gives us an environment variable containing flags
   indicating ARGV elements that should not be considered arguments.  */

static const char *nonoption_flags;
static int nonoption_flags_len;

/* Exchange two adjacent subsequences of ARGV.
   One subsequence is elements [first_nonopt,last_nonopt)
   which contains all the non-options that have been skipped so far.
   The other is elements [last_nonopt,optind), which contains all
   the options processed since those non-options were skipped.

   `first_nonopt' and `last_nonopt' are relocated so that they describe
   the new indices of the non-options in ARGV after they are moved.  */

#if defined (__STDC__) && __STDC__
static void exchange (char **);
#endif

static void
exchange (argv)
     char **argv;
{
  int bottom = first_nonopt;
  int middle = last_nonopt;
  int top = optind;
  char *tem;

  /* Exchange the shorter segment with the far end of the longer segment.
     That puts the shorter segment into the right place.
     It leaves the longer segment in the right place overall,
     but it consists of two parts that need to be swapped next.  */

  while (top > middle && middle > bottom)
    {
      if (top - middle > middle - bottom)
	{
	  /* Bottom segment is the short one.  */
	  int len = middle - bottom;
	  register int i;

	  /* Swap it with the top part of the top segment.  */
	  for (i = 0; i < len; i++)
	    {
	      tem = argv[bottom + i];
	      argv[bottom + i] = argv[top - (middle - bottom) + i];
	      argv[top - (middle - bottom) + i] = tem;
	    }
	  /* Exclude the moved bottom segment from further swapping.  */
	  top -= len;
	}
      else
	{
	  /* Top segment is the short one.  */
	  int len = top - middle;
	  register int i;

	  /* Swap it with the bottom part of the bottom segment.  */
	  for (i = 0; i < len; i++)
	    {
	      tem = argv[bottom + i];
	      argv[bottom + i] = argv[middle + i];
	      argv[middle + i] = tem;
	    }
	  /* Exclude the moved top segment from further swapping.  */
	  bottom += len;
	}
    }

  /* Update records for the slots the non-options now occupy.  */

  first_nonopt += (optind - last_nonopt);
  last_nonopt = optind;
}

/* Initialize the internal data when the first call is made.  */

#if defined (__STDC__) && __STDC__
static const char *_getopt_initialize (const char *);
#endif
static const char *
_getopt_initialize (optstring)
     const char *optstring;
{
  /* Start processing options with ARGV-element 1 (since ARGV-element 0
     is the program name); the sequence of previously skipped
     non-option ARGV-elements is empty.  */

  first_nonopt = last_nonopt = optind = 1;

  nextchar = NULL;

  posixly_correct = getenv ("POSIXLY_CORRECT");

  /* Determine how to handle the ordering of options and nonoptions.  */

  if (optstring[0] == '-')
    {
      ordering = RETURN_IN_ORDER;
      ++optstring;
    }
  else if (optstring[0] == '+')
    {
      ordering = REQUIRE_ORDER;
      ++optstring;
    }
  else if (posixly_correct != NULL)
    ordering = REQUIRE_ORDER;
  else
    ordering = PERMUTE;

  if (posixly_correct == NULL)
    {
      /* Bash 2.0 puts a special variable in the environment for each
	 command it runs, specifying which ARGV elements are the results of
	 file name wildcard expansion and therefore should not be
	 considered as options.  */
      char var[100];
      sprintf (var, "_%d_GNU_nonoption_argv_flags_", getpid ());
      nonoption_flags = getenv (var);
      if (nonoption_flags == NULL)
	nonoption_flags_len = 0;
      else
	nonoption_flags_len = strlen (nonoption_flags);
    }

  return optstring;
}

/* Scan elements of ARGV (whose length is ARGC) for option characters
   given in OPTSTRING.

   If an element of ARGV starts with '-', and is not exactly "-" or "--",
   then it is an option element.  The characters of this element
   (aside from the initial '-') are option characters.  If `getopt'
   is called repeatedly, it returns successively each of the option characters
   from each of the option elements.

   If `getopt' finds another option character, it returns that character,
   updating `optind' and `nextchar' so that the next call to `getopt' can
   resume the scan with the following option character or ARGV-element.

   If there are no more option characters, `getopt' returns `EOF'.
   Then `optind' is the index in ARGV of the first ARGV-element
   that is not an option.  (The ARGV-elements have been permuted
   so that those that are not options now come last.)

   OPTSTRING is a string containing the legitimate option characters.
   If an option character is seen that is not listed in OPTSTRING,
   return '?' after printing an error message.  If you set `opterr' to
   zero, the error message is suppressed but we still return '?'.

   If a char in OPTSTRING is followed by a colon, that means it wants an arg,
   so the following text in the same ARGV-element, or the text of the following
   ARGV-element, is returned in `optarg'.  Two colons mean an option that
   wants an optional arg; if there is text in the current ARGV-element,
   it is returned in `optarg', otherwise `optarg' is set to zero.

   If OPTSTRING starts with `-' or `+', it requests different methods of
   handling the non-option ARGV-elements.
   See the comments about RETURN_IN_ORDER and REQUIRE_ORDER, above.

   Long-named options begin with `--' instead of `-'.
   Their names may be abbreviated as long as the abbreviation is unique
   or is an exact match for some defined option.  If they have an
   argument, it follows the option name in the same ARGV-element, separated
   from the option name by a `=', or else the in next ARGV-element.
   When `getopt' finds a long-named option, it returns 0 if that option's
   `flag' field is nonzero, the value of the option's `val' field
   if the `flag' field is zero.

   The elements of ARGV aren't really const, because we permute them.
   But we pretend they're const in the prototype to be compatible
   with other systems.

   LONGOPTS is a vector of `struct option' terminated by an
   element containing a name which is zero.

   LONGIND returns the index in LONGOPT of the long-named option found.
   It is only valid when a long-named option has been found by the most
   recent call.

   If LONG_ONLY is nonzero, '-' as well as '--' can introduce
   long-named options.  */

int
_getopt_internal (argc, argv, optstring, longopts, longind, long_only)
     int argc;
     char *const *argv;
     const char *optstring;
     const struct option *longopts;
     int *longind;
     int long_only;
{
  optarg = NULL;

  if (optind == 0)
    {
      optstring = _getopt_initialize (optstring);
      optind = 1;		/* Don't scan ARGV[0], the program name.  */
    }

  /* Test whether ARGV[optind] points to a non-option argument.
     Either it does not have option syntax, or there is an environment flag
     from the shell indicating it is not an option.  */
#define NONOPTION_P (argv[optind][0] != '-' || argv[optind][1] == '\0'	      \
		     || (optind < nonoption_flags_len			      \
			 && nonoption_flags[optind] == '1'))

  if (nextchar == NULL || *nextchar == '\0')
    {
      /* Advance to the next ARGV-element.  */

      /* Give FIRST_NONOPT & LAST_NONOPT rational values if OPTIND has been
	 moved back by the user (who may also have changed the arguments).  */
      if (last_nonopt > optind)
	last_nonopt = optind;
      if (first_nonopt > optind)
	first_nonopt = optind;

      if (ordering == PERMUTE)
	{
	  /* If we have just processed some options following some non-options,
	     exchange them so that the options come first.  */

	  if (first_nonopt != last_nonopt && last_nonopt != optind)
	    exchange ((char **) argv);
	  else if (last_nonopt != optind)
	    first_nonopt = optind;

	  /* Skip any additional non-options
	     and extend the range of non-options previously skipped.  */

	  while (optind < argc && NONOPTION_P)
	    optind++;
	  last_nonopt = optind;
	}

      /* The special ARGV-element `--' means premature end of options.
	 Skip it like a null option,
	 then exchange with previous non-options as if it were an option,
	 then skip everything else like a non-option.  */

      if (optind != argc && !strcmp (argv[optind], "--"))
	{
	  optind++;

	  if (first_nonopt != last_nonopt && last_nonopt != optind)
	    exchange ((char **) argv);
	  else if (first_nonopt == last_nonopt)
	    first_nonopt = optind;
	  last_nonopt = argc;

	  optind = argc;
	}

      /* If we have done all the ARGV-elements, stop the scan
	 and back over any non-options that we skipped and permuted.  */

      if (optind == argc)
	{
	  /* Set the next-arg-index to point at the non-options
	     that we previously skipped, so the caller will digest them.  */
	  if (first_nonopt != last_nonopt)
	    optind = first_nonopt;
	  return EOF;
	}

      /* If we have come to a non-option and did not permute it,
	 either stop the scan or describe it to the caller and pass it by.  */

      if (NONOPTION_P)
	{
	  if (ordering == REQUIRE_ORDER)
	    return EOF;
	  optarg = argv[optind++];
	  return 1;
	}

      /* We have found another option-ARGV-element.
	 Skip the initial punctuation.  */

      nextchar = (argv[optind] + 1
		  + (longopts != NULL && argv[optind][1] == '-'));
    }

  /* Decode the current option-ARGV-element.  */

  /* Check whether the ARGV-element is a long option.

     If long_only and the ARGV-element has the form "-f", where f is
     a valid short option, don't consider it an abbreviated form of
     a long option that starts with f.  Otherwise there would be no
     way to give the -f short option.

     On the other hand, if there's a long option "fubar" and
     the ARGV-element is "-fu", do consider that an abbreviation of
     the long option, just like "--fu", and not "-f" with arg "u".

     This distinction seems to be the most useful approach.  */

  if (longopts != NULL
      && (argv[optind][1] == '-'
	  || (long_only && (argv[optind][2] || !my_index (optstring, argv[optind][1])))))
    {
      char *nameend;
      const struct option *p;
      const struct option *pfound = NULL;
      int exact = 0;
      int ambig = 0;
      int indfound;
      int option_index;

      for (nameend = nextchar; *nameend && *nameend != '='; nameend++)
	/* Do nothing.  */ ;

      /* Test all long options for either exact match
	 or abbreviated matches.  */
      for (p = longopts, option_index = 0; p->name; p++, option_index++)
	if (!strncmp (p->name, nextchar, nameend - nextchar))
	  {
	    if (nameend - nextchar == strlen (p->name))
	      {
		/* Exact match found.  */
		pfound = p;
		indfound = option_index;
		exact = 1;
		break;
	      }
	    else if (pfound == NULL)
	      {
		/* First nonexact match found.  */
		pfound = p;
		indfound = option_index;
	      }
	    else
	      /* Second or later nonexact match found.  */
	      ambig = 1;
	  }

      if (ambig && !exact)
	{
	  if (opterr)
	    fprintf (stderr, _("%s: option `%s' is ambiguous\n"),
		     argv[0], argv[optind]);
	  nextchar += strlen (nextchar);
	  optind++;
	  optopt = 0;
	  return '?';
	}

      if (pfound != NULL)
	{
	  option_index = indfound;
	  optind++;
	  if (*nameend)
	    {
	      /* Don't test has_arg with >, because some C compilers don't
		 allow it to be used on enums.  */
	      if (pfound->has_arg)
		optarg = nameend + 1;
	      else
		{
		  if (opterr)
		   if (argv[optind - 1][1] == '-')
		    /* --option */
		    fprintf (stderr,
		     _("%s: option `--%s' doesn't allow an argument\n"),
		     argv[0], pfound->name);
		   else
		    /* +option or -option */
		    fprintf (stderr,
		     _("%s: option `%c%s' doesn't allow an argument\n"),
		     argv[0], argv[optind - 1][0], pfound->name);

		  nextchar += strlen (nextchar);

		  optopt = pfound->val;
		  return '?';
		}
	    }
	  else if (pfound->has_arg == 1)
	    {
	      if (optind < argc)
		optarg = argv[optind++];
	      else
		{
		  if (opterr)
		    fprintf (stderr,
			   _("%s: option `%s' requires an argument\n"),
			   argv[0], argv[optind - 1]);
		  nextchar += strlen (nextchar);
		  optopt = pfound->val;
		  return optstring[0] == ':' ? ':' : '?';
		}
	    }
	  nextchar += strlen (nextchar);
	  if (longind != NULL)
	    *longind = option_index;
	  if (pfound->flag)
	    {
	      *(pfound->flag) = pfound->val;
	      return 0;
	    }
	  return pfound->val;
	}

      /* Can't find it as a long option.  If this is not getopt_long_only,
	 or the option starts with '--' or is not a valid short
	 option, then it's an error.
	 Otherwise interpret it as a short option.  */
      if (!long_only || argv[optind][1] == '-'
	  || my_index (optstring, *nextchar) == NULL)
	{
	  if (opterr)
	    {
	      if (argv[optind][1] == '-')
		/* --option */
		fprintf (stderr, _("%s: unrecognized option `--%s'\n"),
			 argv[0], nextchar);
	      else
		/* +option or -option */
		fprintf (stderr, _("%s: unrecognized option `%c%s'\n"),
			 argv[0], argv[optind][0], nextchar);
	    }
	  nextchar = (char *) "";
	  optind++;
	  optopt = 0;
	  return '?';
	}
    }

  /* Look at and handle the next short option-character.  */

  {
    char c = *nextchar++;
    char *temp = my_index (optstring, c);

    /* Increment `optind' when we start to process its last character.  */
    if (*nextchar == '\0')
      ++optind;

    if (temp == NULL || c == ':')
      {
	if (opterr)
	  {
	    if (posixly_correct)
	      /* 1003.2 specifies the format of this message.  */
	      fprintf (stderr, _("%s: illegal option -- %c\n"),
		       argv[0], c);
	    else
	      fprintf (stderr, _("%s: invalid option -- %c\n"),
		       argv[0], c);
	  }
	optopt = c;
	return '?';
      }
    if (temp[1] == ':')
      {
	if (temp[2] == ':')
	  {
	    /* This is an option that accepts an argument optionally.  */
	    if (*nextchar != '\0')
	      {
		optarg = nextchar;
		optind++;
	      }
	    else
	      optarg = NULL;
	    nextchar = NULL;
	  }
	else
	  {
	    /* This is an option that requires an argument.  */
	    if (*nextchar != '\0')
	      {
		optarg = nextchar;
		/* If we end this ARGV-element by taking the rest as an arg,
		   we must advance to the next element now.  */
		optind++;
	      }
	    else if (optind == argc)
	      {
		if (opterr)
		  {
		    /* 1003.2 specifies the format of this message.  */
		    fprintf (stderr,
			   _("%s: option requires an argument -- %c\n"),
			   argv[0], c);
		  }
		optopt = c;
		if (optstring[0] == ':')
		  c = ':';
		else
		  c = '?';
	      }
	    else
	      /* We already incremented `optind' once;
		 increment it again when taking next ARGV-elt as argument.  */
	      optarg = argv[optind++];
	    nextchar = NULL;
	  }
      }
    return c;
  }
}

int
getopt (argc, argv, optstring)
     int argc;
     char *const *argv;
     const char *optstring;
{
  return _getopt_internal (argc, argv, optstring,
			   (const struct option *) 0,
			   (int *) 0,
			   0);
}

#endif	/* _LIBC or not __GNU_LIBRARY__.  */

#ifdef TEST

/* Compile with -DTEST to make an executable for use in testing
   the above definition of `getopt'.  */

int
main (argc, argv)
     int argc;
     char **argv;
{
  int c;
  int digit_optind = 0;

  while (1)
    {
      int this_option_optind = optind ? optind : 1;

      c = getopt (argc, argv, "abc:d:0123456789");
      if (c == EOF)
	break;

      switch (c)
	{
	case '0': case_coverage = case_coverage;
	case '1': case_coverage = case_coverage;
	case '2': case_coverage = case_coverage;
	case '3': case_coverage = case_coverage;
	case '4': case_coverage = case_coverage;
	case '5': case_coverage = case_coverage;
	case '6': case_coverage = case_coverage;
	case '7': case_coverage = case_coverage;
	case '8': case_coverage = case_coverage;
	case '9':
	  if (digit_optind != 0 && digit_optind != this_option_optind)
	    printf ("digits occur in two different argv-elements.\n");
	  digit_optind = this_option_optind;
	  printf ("option %c\n", c);
	  break;

	case 'a':
	  printf ("option a\n");
	  break;

	case 'b':
	  printf ("option b\n");
	  break;

	case 'c':
	  printf ("option c with value `%s'\n", optarg);
	  break;

	case '?':
	  break;

	default:
	  printf ("?? getopt returned character code 0%o ??\n", c);
	}
    }

  if (optind < argc)
    {
      printf ("non-option ARGV-elements: ");
      while (optind < argc)
	printf ("%s ", argv[optind++]);
      printf ("\n");
    }

  exit (0);
}

#endif /* TEST */



/*============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================
                         getopt1.c
  ============================================================
  ************************************************************
  ============================================================
  ************************************************************
  ============================================================*/

/* getopt_long and getopt_long_only entry points for GNU getopt.
   Copyright (C) 1987, 88, 89, 90, 91, 92, 1993, 1994
	Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
   USA.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#undef stderr
#define stderr stdout
#endif

#include "getopt.h"
#undef stderr
#define stderr stdout

#if !defined (__STDC__) || !__STDC__
/* This is a separate conditional since some stdc systems
   reject `defined (const)'.  */
#ifndef const
#define const
#endif
#endif

#include <stdio.h>
#undef stderr
#define stderr stdout

/* Comment out all this code if we are using the GNU C Library, and are not
   actually compiling the library itself.  This code is part of the GNU C
   Library, but also included in many other GNU distributions.  Compiling
   and linking in this code is a waste when using the GNU C library
   (especially if it is a shared library).  Rather than having every GNU
   program understand `configure --with-gnu-libc' and omit the object files,
   it is simpler to just do this in the source for each such file.  */

#if defined (_LIBC) || !defined (__GNU_LIBRARY__)


/* This needs to come after some library #include
   to get __GNU_LIBRARY__ defined.  */
#ifdef __GNU_LIBRARY__
#include <stdlib.h>
#undef stderr
#define stderr stdout
#else
char *getenv ();
#endif

#ifndef	NULL
#define NULL 0
#endif

int
getopt_long (argc, argv, options, long_options, opt_index)
     int argc;
     char *const *argv;
     const char *options;
     const struct option *long_options;
     int *opt_index;
{
  return _getopt_internal (argc, argv, options, long_options, opt_index, 0);
}

/* Like getopt_long, but '-' as well as '--' can indicate a long option.
   If an option that starts with '-' (not '--') doesn't match a long option,
   but does match a short option, it is parsed as a short option
   instead.  */

int
getopt_long_only (argc, argv, options, long_options, opt_index)
     int argc;
     char *const *argv;
     const char *options;
     const struct option *long_options;
     int *opt_index;
{
  return _getopt_internal (argc, argv, options, long_options, opt_index, 1);
}


#endif	/* _LIBC or not __GNU_LIBRARY__.  */

#ifdef TEST

#include <stdio.h>
#undef stderr
#define stderr stdout

int
main (argc, argv)
     int argc;
     char **argv;
{
  int c;
  int digit_optind = 0;

  while (1)
    {
      int this_option_optind = optind ? optind : 1;
      int option_index = 0;
      static struct option long_options[] =
      {
	{"add", 1, 0, 0},
	{"append", 0, 0, 0},
	{"delete", 1, 0, 0},
	{"verbose", 0, 0, 0},
	{"create", 0, 0, 0},
	{"file", 1, 0, 0},
	{0, 0, 0, 0}
      };

      c = getopt_long (argc, argv, "abc:d:0123456789",
		       long_options, &option_index);
      if (c == EOF)
	break;

      switch (c)
	{
	case 0:
	  printf ("option %s", long_options[option_index].name);
	  if (optarg)
	    printf (" with arg %s", optarg);
	  printf ("\n");
	  break;

	case '0': case_coverage = case_coverage;
	case '1': case_coverage = case_coverage;
	case '2': case_coverage = case_coverage;
	case '3': case_coverage = case_coverage;
	case '4': case_coverage = case_coverage;
	case '5': case_coverage = case_coverage;
	case '6': case_coverage = case_coverage;
	case '7': case_coverage = case_coverage;
	case '8': case_coverage = case_coverage;
	case '9':
	  if (digit_optind != 0 && digit_optind != this_option_optind)
	    printf ("digits occur in two different argv-elements.\n");
	  digit_optind = this_option_optind;
	  printf ("option %c\n", c);
	  break;

	case 'a':
	  printf ("option a\n");
	  break;

	case 'b':
	  printf ("option b\n");
	  break;

	case 'c':
	  printf ("option c with value `%s'\n", optarg);
	  break;

	case 'd':
	  printf ("option d with value `%s'\n", optarg);
	  break;

	case '?':
	  break;

	default:
	  printf ("?? getopt returned character code 0%o ??\n", c);
	}
    }

  if (optind < argc)
    {
      printf ("non-option ARGV-elements: ");
      while (optind < argc)
	printf ("%s ", argv[optind++]);
      printf ("\n");
    }

  exit (0);
}

#endif /* TEST */
