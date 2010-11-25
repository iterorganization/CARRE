# Simple F77->F90 conversion
# Does only comments and line continuation
# Handles comment lines, embedded comments and preprocessor directives

# The script handles embedded comments in continued lines and tests whether 
# the comment indicator '!' is located inside a string literal. 
#
# Example: 
#             write(*,*) 'Warning!  ',
#      1       'The gas puff was started with an initial rate of 1e18!'
#
#             write(*,*) 'Warning!  ', & 
#      &       'The gas puff was started with an initial rate of 1e18!'
#
# It fails for more complicated situations with escaped characters. 
# It definitely fails if the embedded comment contains either ' or ". 
# The fail mode in such a case should lead to a compile time error.

import sys

if len(sys.argv) != 3:
    print "You have to supply two filenames"
    raise SystemExit(1)

CCHAR = 'cC*!'    # Comment characters
PCHAR = '#@'      # Preprocessor directive characters

# read into buffer

f77 = open(sys.argv[1])
lines = f77.readlines()
f77.close()


# modify

i = 0   # index of current line
ill = 0 # index of last statement line

for l in lines:

    if CCHAR.count( l[0] ) > 0: 
        # Comment line, fix comment character
        lines[i] = '!' + l[1:]
    elif PCHAR.count( l[0] ) > 0:
        # Preprocessor directive, skip
        pass
    else:
        # statement line, check for continuation
        if ( len( l ) > 4 ) and ( l[5] != ' ' ):
            # continuation. Fix continuation character...
            lines[i] = l[0:5] + '&' + l[6:]
            # ...and add one at the end of the previous statement
            linedone = False
            cind = 0
            while ( not linedone ):
                # check for embedded comment
                cind = lines[ill].rfind('!', cind)
                if ( cind != -1 ):
                    # Looks like a continued line with embedded comment.
                    # Quick and dirty check whether the ! is in a literal constant
                    sind1 = lines[ill].find('\'', cind) 
                    sind2 = lines[ill].find('\"', cind) 
                    if ( sind1 == -1 ) and ( sind2 == -1 ):
                        # doesn't seem to be the case, put & in front of current !
                        linedone = True
                    else:
                    # ! seems to be in a literal constant, look for next !
                        cind = max( sind1, sind2 ) + 1
                        #cind = cind + 1
                        pass
                else:
                    # none found, put at end of line
                    linedone = True
                
            # insert continuation symbol
            lines[ill] = lines[ill][:cind] + ' & ' + lines[ill][cind:]

        # make this the last statement line
        ill = i

    i = i + 1

# write out 

f90 = open(sys.argv[2], 'w')
f90.writelines(lines)
f90.close()
