! bof
! **********************************************************************
! Fortran 95 program preconvert

! **********************************************************************
! Revision Control Strings

! $Id: preconvert.f90 1.6 2003/11/06 14:01:28Z Dan Release $

! **********************************************************************
!  Copyright 2001 Purple Sage Computing Solutions, Inc.

!   This program is free software; you can redistribute it and/or
!   modify it under the terms of the GNU General Public
!   License as published by the Free Software Foundation; either
!   version 2 of the License, or (at your option) any later version.

!   This library is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   Library General Public License for more details.

!   You should have received a copy of the GNU General Public
!   License along with this library; if not, write to the Free
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, etc. to the Authors,
! Contact:
!    Purple Sage Computing Solutions, Inc.
!                               send email to dnagle@erols.com
!                                   or fax to 703 471 0684 (USA)
!                                  or mail to 12142 Purple Sage Ct.
!                                             Reston, VA 20194-5621 USA

! **********************************************************************
! preconvert preprocessing before use of convert

!  use

!  $ preconvert
!  Enter source filename (without .for suffix),
!  followed by T or F for tab removal,
!  followed by T or F to convert debug lines to comments,
!  followed by T or F to mark include files: <your response here>
!  followed by a string indicating which preprocessor format you want

!  The string can be: "coco" or "f90ppr" or "fpp"

!  Your response might be:

!  1. to read 'what.for', writing 'what.f', with tab removal, converting
!     debug lines to comments and marking include files, no conditional
!     compilation directives:
!     what /

!  2. to read 'filename.for', writing 'filename.f', no tab removal,
!     not converting debug lines to comments, and not marking include files
!     and no conditional compilation directives:
!     filename f f f

!  3. to read 'prog.for', writing 'prog.f', with tab removal, process
!     debug lines via coco conditional compilation directives, marking
!     include files:
!     prog t t t coco

! **********************************************************************

!  preconvert reads

!     unput_unit- filenames, flags
!     infn- fixed format source to be processed

!  preconvert writes

!     output_unit- banner & summary
!     outfn- processed fixed format source
!     logfn- logfile of actions and complaints

!  preconvert uses

!     <none>

!  preconvert constants

!     incunit= first include file unit
!     logfile= log file unit
!     strings_len= length of character string variables
!     beg_inc= string marking the beginning of included include files
!     end_inc= string marking the end of included include files
!     blank= character blank
!     tab= character tab
!     null_string= the null character string

!  preconvert types

!     <none>

!  preconvert data

!     files= total files processed
!     lines= total lines processed
!     code_lines= lines of code processed
!     tabs= number of tabs removed

!  preconvert library

!     copyfile() processes one file
!     debug_line() convert a debug line to a comment line
!     bclc() blank compresses a string and converts it to lower case
!     get_inc_name() gets an include file's name from an include statement
!     remove_tabs() removes tabs from a fixed format line

! **********************************************************************

!  preconvert

! **********************************************************************

program preconvert                                         ! preconvert

! **********************************************************************

!  preconvert uses programs

! **********************************************************************

!  no modules used

! **********************************************************************

!  no implicit declarations

implicit none

! **********************************************************************

!  preconvert rcs strings

! **********************************************************************

!  file identifier

character( len= *), parameter :: preconvert_rcs_id = &               ! filled in by RCS
   '$Id: preconvert.f90 1.6 2003/11/06 14:01:28Z Dan Release $'

! **********************************************************************

!  preconvert constants

! **********************************************************************

!  io units

integer, parameter :: incunit = 11                                   ! first open include file

integer, parameter :: outunit = 10                                   ! source output unit
integer, parameter :: inunit = 9                                     ! source input unit

integer, parameter :: logfile = 8                                    ! log file unit

integer, parameter :: input_unit = 5                                 ! or use f03 iso_fortran env
integer, parameter :: output_unit = 6                                ! or use f03 iso_fortran env

! **********************************************************************

!  file name constants

! **********************************************************************

!  name of logfile

character( len= *), parameter :: logsuffix = '.log'                  ! <name>.log

!  input and output filename suffixes

character( len= *), parameter :: insuffix = '.for'                   ! <name>.for
character( len= *), parameter :: outsuffix = '.f'                    ! <name>.f

!  mark beginning and end of include files

character( len= *), parameter :: beg_inc = '* '                      ! * include 'foo'
character( len= *), parameter :: end_inc = '* end '                  ! * end include 'foo'

! **********************************************************************

!  preprocessor names (which have different formats)

character( len= *), parameter :: coco_str = 'coco'                   ! use coco format

integer, parameter :: coco_len = len( coco_str)

character( len= *), parameter :: f90ppr_str = 'f90ppr'               ! use f90ppr format

integer, parameter :: f90ppr_len = len( f90ppr_str)

character( len= *), parameter :: fpp_str = 'fpp'                     ! use fpp format

integer, parameter :: fpp_len = len( fpp_str)

integer, parameter :: max_ppr_len = max( coco_len, f90ppr_len, fpp_len)

!  codes for the above

integer, parameter :: no_ppr = 0                                     ! no preprocessor

integer, parameter :: coco_ppr = 1                                   ! coco format

integer, parameter :: f90ppr_ppr = 2                                 ! f90ppr format

integer, parameter :: fpp_ppr = 3                                    ! fpp format

! **********************************************************************

!  length of most strings

integer, parameter :: strings_len = 512                              ! longer than standard lines

!  character constants

character, parameter :: blank = ' '
character, parameter :: tab = achar( 9)                              ! use ascii

character, parameter :: null_string = ''

! **********************************************************************

!  preconvert data

! **********************************************************************

!  counters

integer :: files = 0                                                 ! count all files
integer :: lines = 0                                                 ! count all lines
integer :: code_lines = 0                                            ! count code lines
integer :: tabs = 0                                                  ! count tabs

!  input and output filenames

character( len= strings_len) :: basefn                               ! name
character( len= strings_len) :: infn                                 ! name.for
character( len= strings_len) :: outfn                                ! name.f

character( len= strings_len) :: logfn                                ! name.log

character( len= max_ppr_len) :: ppr = null_string                    ! name of preprocessor

!  whether to remove tabs & convert debug lines to comments

logical :: do_tabs = .true.                                          ! remove tabs
logical :: do_debug = .true.                                         ! convert debug lines to comments
logical :: mark_inc = .true.                                         ! mark include files

integer :: ppr_code = no_ppr                                         ! no preprocessor

logical :: in_d_sequence = .false.                                   ! not (yet) in debug comments

!  flag is true until an error is encounterd

logical :: global_aok = .true.                                       ! true until an error

! **********************************************************************

!  preconvert text

! **********************************************************************

continue                                                             ! preconvert

!  write banner

   write( unit= output_unit, fmt= *) preconvert_rcs_id
   write( unit= output_unit, fmt= *)

!  get input and output filenames

   write( unit= output_unit, fmt= *) 'Enter source filename (without .for suffix),'
   write( unit= output_unit, fmt= *) 'followed by T or F for tab removal,'
   write( unit= output_unit, fmt= *) 'followed by T or F to convert debug lines to comments,'
   write( unit= output_unit, fmt= *) 'followed by T or F to mark include files,'
   write( unit= output_unit, fmt= *) 'followed by "coco", "f90ppr" or "fpp" preprocessor: '

   read( unit= input_unit, fmt= *) basefn, do_tabs, do_debug, mark_inc, ppr

   outfn = trim( basefn) // outsuffix                                ! output to name.f
   infn = trim( basefn) // insuffix                                  ! input from name.for

!  open logfile

   logfn = trim( basefn) // logsuffix                                ! log file name
   open( unit= logfile, file= logfn, status= 'replace')

   write( unit= logfile, fmt= *) ' input: ', trim( infn), ' output: ', trim( outfn)

!  open input and output files

   open( unit= inunit, file= infn, status= 'old')
   open( unit= outunit, file= outfn, status= 'replace')

!  set up preprocessor code & write symbol definition line

   set_ppr: select case( ppr)

!  set up for coco

   case( coco_str) set_ppr

      ppr_code = coco_ppr

!  define debug symbol coco style

      write( unit= outunit, fmt= '(a)') '*?>?? logical, parameter :: debug = .false.'

!  set up for f90ppr

   case( f90ppr_str) set_ppr

      ppr_code = f90ppr_ppr

!  define debug symbol f90ppr style

      write( unit= outunit, fmt= '(a)') '*$define debug'

!  set up for fpp

   case( fpp_str) set_ppr

      ppr_code = fpp_ppr

!  define debug symbol fpp style

      write( unit= outunit, fmt= '(a)') '*#define debug'

!  ignore unknown preprocessors

   case default set_ppr

      write( unit= output_unit, fmt= *) ' unrecognized preprocessor: ', trim( ppr)
      write( unit= logfile, fmt= *) ' unrecognized preprocessor: ', trim( ppr)

   end select set_ppr

!  read fixed format source

   call copyfile( inunit)                                            ! copy with input file

!  write to logfile

   write( unit= logfile, fmt= *) ' files: ', files, ' lines: ', lines, &
                                 ' code: ', code_lines, ' tabs: ', tabs

!  close files

   close( unit= logfile)                                             ! logfile
   close( unit= inunit)                                              ! input file
   close( unit= outunit)                                             ! output file

!  write summary

   write( unit= output_unit, fmt= *) ' input: ', trim( infn), ' output: ', trim( outfn)

   write( unit= output_unit, fmt= *) ' files: ', files               ! total files
   write( unit= output_unit, fmt= *) ' lines: ', lines               ! total lines
   write( unit= output_unit, fmt= *) ' code: ', code_lines           ! code lines
   write( unit= output_unit, fmt= *) ' tabs: ', tabs                 ! total tabs

!  indicate if trouble encounterd

   write( unit= output_unit, fmt= *) 'Global success flag: ', global_aok

!  exit preconvert

stop 'preconvert normal exit'                                        ! preconvert

! **********************************************************************

!  preconvert library

! **********************************************************************

contains                                                             ! preconvert

! **********************************************************************

!  reads a fixed format source file, recurse upon include files

recursive subroutine copyfile( iunit)

!  copyfile() interface

integer, intent( in) :: iunit

! **********************************************************************

!  formats

   character( len= *), parameter :: in_fmt = '(a)'
   character( len= *), parameter :: out_fmt = '(a)'

! ----------------------------------------------------------------------

!  copyfile() data

   character( len= strings_len) :: buffer                            ! store a line

   character( len= strings_len) :: bclcbuff                          ! a blank compressed line

   character( len= strings_len) :: fname                             ! name of include file

!  local

   integer :: ist, new_unit

! **********************************************************************

!  copyfile() text

continue                                                             ! copyfile()

!  count files

   files = files + 1                                                 ! one file per execution of copyfile()

! ----------------------------------------------------------------------

!  main read lines loop

   read_lines: do                                                    ! exit upon end-of-file

      read( unit= iunit, fmt= in_fmt, iostat= ist) buffer            ! line into buffer

      if( ist < 0 ) exit read_lines                                  ! exit when end-of-file

!  count lines

      lines = lines + 1                                              ! one line per iteration

!  change debug lines to comments

      if( do_debug ) call debug_line( buffer)                        ! if converting debug lines

!  comment lines will be copied but otherwise ignored

      ignore_comments: if( scan( buffer( 1: 1), 'Cc*') == 0 .and. &
                           buffer /= blank )then

!  count code lines

         code_lines = code_lines + 1                                 ! one more code line

!  blank compress to find include statement

         call bclc( bclcbuff, buffer)                                ! blank compress & lower case

!  find include statements

         get_include: if( bclcbuff( 1: 8) == 'include"' .or. &
            bclcbuff( 1: 8) == "include'" )then

!  found include statement

            call get_inc_name( fname, bclcbuff)                      ! get include file name

            no_name: if( fname == blank )then                        ! no include file name

               write( unit= logfile, fmt= *) 'no name: ', trim( buffer)

               write( unit= outunit, fmt= out_fmt) trim( buffer)

               global_aok = .false.                                  ! trouble encountered

               cycle read_lines                                      ! go read next line

            endif no_name                                            ! no inlcude file name

!  get new unit

            which_unit: if( iunit == inunit )then

               new_unit = incunit                                    ! first include file

            else which_unit                                          ! unit to open include file

               new_unit = iunit + 1                                  ! nested include file

            endif which_unit                                         ! unit to open include file

!  open the include file

            open( unit= new_unit, file= fname, status= 'old', iostat= ist)

!  if open failed, complain and ignore

            open_failed: if( ist > 0 )then

               write( unit= logfile, fmt= '(a, 2i5)') trim(  "open: " // fname), new_unit, ist

               write( unit= logfile, fmt= *) trim( buffer)

               global_aok = .false.                                  ! trouble encountered

               cycle read_lines                                      ! go read next line

            endif open_failed

!  open succeeds, go process include file

            write( unit= logfile, fmt= *) 'include ' // trim( fname)

            if( mark_inc ) write( unit= outunit, fmt= out_fmt) trim( beg_inc // adjustl( buffer))

            call copyfile( new_unit)                                 ! process include file

            if( mark_inc ) write( unit= outunit, fmt= out_fmt) trim( end_inc // adjustl( buffer))

            close( unit= new_unit)                                   ! close include file

            cycle read_lines                                         ! go read next line

!  end processing include statement

         endif get_include

!  non-comment, non-include statement lines have tabs removed

         if( do_tabs ) call remove_tabs( buffer)                     ! remove tabs from line

!  end processing non-comments

      endif ignore_comments

!  all comment and statement lines

      write( unit= outunit, fmt= out_fmt) trim( buffer)

!  end main read lines loop

   enddo read_lines                                                  ! exit upon end-of-file

! ----------------------------------------------------------------------

!  end of file

return                                                               ! copyfile()

!  copyfile()

end subroutine copyfile                                              ! copyfile()

! **********************************************************************

!  convert debug line to comment

subroutine debug_line( line)

!  debug_line() interface

character( len= *), intent( inout) :: line

! **********************************************************************

!  debug_line() text

continue                                                             ! debug_line()

   d_lines: select case( ppr_code)

   case( no_ppr) d_lines

      change: if( line( 1: 1) == 'D' )then                           ! debug line

         line( 1: 1) = 'C'                                           ! D --> C

      elseif( line( 1: 1) == 'd' )then change                        ! debug line

         line( 1: 1) = 'c'                                           ! d --> c

      endif change                                                   ! debug line

   case( coco_ppr) d_lines

      call go_coco( line)

   case( f90ppr_ppr) d_lines

      call go_f90ppr( line)

   case( fpp_ppr) d_lines

      call go_fpp( line)

   end select d_lines

return                                                               ! debug_line()

!  debug_line()

end subroutine debug_line                                            ! debug_line()

! **********************************************************************

!  blank compress and convert to lower case

subroutine bclc( outstr, instr)

!  bclc() interface

character( len= *), intent( in) :: instr

character( len= *), intent( out) :: outstr

! **********************************************************************

!  bclc() local

   integer, parameter :: change_case = 32                            ! use achar()/iachar()

   integer :: in_ptr, out_ptr, index_quote

! **********************************************************************

!  bclc() text

continue                                                             ! bclc()

!  initialize

   outstr = null_string                                              ! output string

   in_ptr = 1                                                        ! first character of input
   out_ptr = 1                                                       ! first character of output

!  do each character until end of input string

   each_char: do while( in_ptr <= len_trim( instr))

!  character is not a blank (or tab)

      non_blanks: if( instr( in_ptr: in_ptr) /= blank .and. instr( in_ptr: in_ptr) /= tab )then

!  character is single quote
 
         sq_string: if( instr( in_ptr: in_ptr) == "'" )then

            index_quote = index( instr( in_ptr+1: ), "'")            ! find matching single quote

            found_sq: if( index_quote > 0 )then                      ! found matching single quote

               outstr( out_ptr: out_ptr+index_quote) = instr( in_ptr: in_ptr+index_quote)

               in_ptr = in_ptr + index_quote                         ! update in pointer
               out_ptr = out_ptr + index_quote                       ! update out pointer

               cycle each_char                                       ! go next character

            endif found_sq                                           ! found matching single quote

         endif sq_string

!  character is double quote

         dq_string: if( instr( in_ptr: in_ptr) == '"' )then

            index_quote = index( instr( in_ptr+1: ), '"')            ! find matching double quote

            found_dq: if( index_quote > 0 )then                      ! found matching double quote

               outstr( out_ptr: out_ptr+index_quote) = instr( in_ptr: in_ptr+index_quote)

               in_ptr = in_ptr + index_quote                         ! update in pointer
               out_ptr = out_ptr + index_quote                       ! update out pointer

               cycle each_char                                       ! go next character

            endif found_dq                                           ! found matching double quote

         endif dq_string

!  copy the non-blank character

         outstr( out_ptr: out_ptr) = instr( in_ptr: in_ptr)

!  check for upper case

         uc_to_lc: if( outstr( out_ptr: out_ptr) >= 'A' .and. outstr( out_ptr: out_ptr) <= 'Z' )then

            outstr( out_ptr: out_ptr) = achar( iachar( outstr( out_ptr: out_ptr)) + change_case)

         endif uc_to_lc

!  update out pointer

         out_ptr = out_ptr + 1

      endif non_blanks

!  update in pointer

      in_ptr = in_ptr + 1

   enddo each_char

return                                                               ! bclc()

!  bclc()

end subroutine bclc                                                  ! bclc()

! **********************************************************************

!  get include file name

subroutine get_inc_name( outstr, instr)

!  get_inc_name() interface

character( len= *), intent( in) :: instr

character( len= *), intent( out) :: outstr

! **********************************************************************

!  get_inc_name() local

   integer, parameter :: len_mark = len_trim( 'include') + 1
   integer, parameter :: len_markp1 = len_mark + 1, len_markm1 = len_mark - 1

   integer :: index_quote

! **********************************************************************

!  get_inc_name() text

continue                                                             ! get_inc_name()

!  find single quote or double quote

   which_quote: if( instr( len_mark: len_mark) == "'" )then

      index_quote = index( instr( len_markp1: ), "'")                ! find single quote

   elseif( instr( len_mark: len_mark) == '"' )then which_quote

      index_quote = index( instr( len_markp1: ), '"')                ! find double quote

   else which_quote

      index_quote = 0                                      ! no quotes
      write( unit= logfile, fmt= *) 'no quote: ' // trim( instr)

   endif which_quote

!  find matching quote

   no_match: if( index_quote > 1 )then                               ! found matching quote

      outstr = instr( len_markp1: len_markm1+index_quote)            ! output between quotes

   else no_match                                                     ! no matching quote

      write( unit= logfile, fmt= *) 'no matching quote: ' // trim( instr)

      outstr = null_string                                           ! no output

   endif no_match

!  return with name

return                                                               ! get_inc_name()

!  get_inc_name()

end subroutine get_inc_name                                          ! get_inc_name

! **********************************************************************

!  remove tab characters

subroutine remove_tabs( line)

!  remove_tabs() interface

character( len= *), intent( inout) :: line                           ! rewrite line w/o tabs

! **********************************************************************

!  remove_tabs() local

   character( len= 6), parameter :: blanks = '      '

   integer :: icol, index_quote

! **********************************************************************

!  remove_tabs() text

continue

! ----------------------------------------------------------------------

!  initial tab is different

      initial_ch: if( line( 1: 1) == tab )then                       ! initial tab

!  initial character is tab

         tabs = tabs + 1                                             ! count tabs

!  <tab>-<digit> is a continuation line

         stmt_num: if( line(2: 2) >= '1' .and. line( 2: 2) <= '9' )then

!  tab --> blanks in columns 1 thru 5

            line = blanks( 1: 5) // line( 2: )                       ! places digit in column 6

!  <tab>-<other> is an initial line

         else stmt_num

!  tab --> blanks in columns 1 thru 6

            line = blanks // line( 2: )                              ! places next character in column 7

         endif stmt_num

!  initial character is not a tab

      else initial_ch                                                ! initial not tab

!  check for initial line number then tab

         sn_field: do icol = 1, 6                                    ! search columns 1 thru 6

!  skip past digits

            if( line( icol: icol) >= '0' .and. line( icol: icol) <= '9' ) cycle sn_field

!  if tab is found, fill out columns 1 thru 6

            found_tab: if( line( icol: icol) == tab )then

!  tab in columns 2 thru 6

               tabs = tabs + 1                                       ! count tabs

!  insert blanks from tab's position to column 6

               line = line( : icol-1) //  blanks( icol: ) // line( icol+1: )

!  quit once past column 6

               exit sn_field                                         ! now past column 6

            endif found_tab                                          ! filled out columns 1 thru 6

         enddo sn_field                                              ! search columns 1 thru 6

      endif initial_ch                                               ! initial is/is not tab

! ----------------------------------------------------------------------

!  loop thru rest of statement

      icol = 7                                                       ! start at beginning of statement

      each_ch: do while( icol <= len_trim( line))

!  skip past single quoted literal strings

         sq_string: if( line( icol: icol) == "'" )then

!  if second ' on line

            index_quote = index( line( icol+1: ), "'")

            have_sq: if( index_quote > 0 )then                       ! have a second ' on line

!  skip past it

               icol = icol + index_quote + 1                         ! point to character after '

            endif have_sq                                            ! have a second ' on line

         endif sq_string

!  skip past double quoted literal strings

         dq_string: if( line( icol: icol) == '"' )then

!  if second " on line

            index_quote = index( line( icol+1: ), '"')

            have_dq: if( index_quote > 0 )then                       ! have a second " on line

!  skip past it

               icol = icol + index_quote + 1                         ! point to character after "

            endif have_dq                                            ! have a second " on line

         endif dq_string

!  if character is a tab

         tab_ch: if( line( icol: icol) == tab )then

!  process tab

            tabs = tabs + 1                                          !  count tabs

!  tabs become blanks past column 7

            line( icol: icol) = blank

         endif tab_ch

         icol = icol + 1                                             ! next character

      enddo each_ch

return                                                               ! remove_tabs()

! ----------------------------------------------------------------------

!  remove_tabs()

end subroutine remove_tabs                                           ! remove_tabs

! **********************************************************************

!  convert debug line to comment

subroutine go_coco( line)

!  go_coco() interface

character( len= *), intent( inout) :: line

! **********************************************************************

!  go_coco() text

continue                                                             ! go_coco()

   d_where: if( .not. in_d_sequence .and. &
              ( line( 1: 1) == 'D' .or. line( 1: 1) == 'd') )then

      in_d_sequence = .true.

      write( unit= outunit, fmt= '(a)') '*?>?? if( debug )then'

      line( 1: 1) = ' '

   elseif( in_d_sequence .and. &
         .not. ( line( 1: 1) == 'D' .or. line( 1: 1) == 'd' ) )then d_where

      in_d_sequence = .false.

      write( unit= outunit, fmt= '(a)') '*?>?? endif'

   elseif( line( 1: 1) == 'D' .or. line( 1: 1) == 'd' )then d_where

      line( 1: 1) = ' '

   endif d_where

return                                                               ! go_coco()

!  go_coco()

end subroutine go_coco                                               ! go_coco()

! **********************************************************************

!  convert debug line to comment

subroutine go_f90ppr( line)

!  go_f90ppr() interface

character( len= *), intent( inout) :: line

! **********************************************************************

!  go_f90ppr() text

continue                                                             ! go_f90ppr()

   d_where: if( .not. in_d_sequence .and. &
              ( line( 1: 1) == 'D' .or. line( 1: 1) == 'd') )then

      in_d_sequence = .true.

      write( unit= outunit, fmt= '(a)') '*$ifdef debug'

      line( 1: 1) = ' '

   elseif( in_d_sequence .and. &
         .not. ( line( 1: 1) == 'D' .or. line( 1: 1) == 'd' ) )then d_where

      in_d_sequence = .false.

      write( unit= outunit, fmt= '(a)') '*$endif'

   elseif( line( 1: 1) == 'D' .or. line( 1: 1) == 'd' )then d_where

      line( 1: 1) = ' '

   endif d_where

return                                                               ! go_f90ppr()

!  go_f90ppr()

end subroutine go_f90ppr                                             ! go_f90ppr()

! **********************************************************************

!  convert debug line to comment

subroutine go_fpp( line)

!  go_fpp() interface

character( len= *), intent( inout) :: line

! **********************************************************************

!  go_fpp() text

continue                                                             ! go_fpp()

   d_where: if( .not. in_d_sequence .and. &
              ( line( 1: 1) == 'D' .or. line( 1: 1) == 'd') )then

      in_d_sequence = .true.

      write( unit= outunit, fmt= '(a)') '*#ifdef debug'

      line( 1: 1) = ' '

   elseif( in_d_sequence .and. &
         .not. ( line( 1: 1) == 'D' .or. line( 1: 1) == 'd' ) )then d_where

      in_d_sequence = .false.

      write( unit= outunit, fmt= '(a)') '*#endif'

   elseif( line( 1: 1) == 'D' .or. line( 1: 1) == 'd' )then d_where

      line( 1: 1) = ' '

   endif d_where

return                                                               ! go_fpp()

!  go_fpp()

end subroutine go_fpp                                                ! go_fpp()

! **********************************************************************

!  preconvert

! $Id: preconvert.f90 1.6 2003/11/06 14:01:28Z Dan Release $
! **********************************************************************

end program preconvert                                               ! eof
