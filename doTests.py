from os import system

def cleanup():
    system('rm -f *.tmp a.out')

## Open files to test
fd = open('testFiles.txt')
s = fd.read().split('\n')

## Make MIPS compiler
system('make')


for i in s:
    raw_input('Press any key to run test.')
    print 'Running test: ', i
    cleanup()
    ## Compile and run a c version of the code
    system('gcc ./TESTING_C/' + i + ' 2> /dev/null')
    system('./a.out > cout.tmp')
    ## Compile and run a mips version of the code
    system('java P6 ./TESTING_C/' + i + ' spim.tmp 2> /dev/null')
    system('spim spim.tmp > spimout.tmp')

    ## Remove header from output
    fd = open('spimout.tmp')
    of = open('spimout_clean.tmp', 'w')
    t = fd.read().split('\n')
    for j in t[5:]:
        of.write(j + '\n')
    of.close()

    ## Compare outputs
    system('diff -b -B spimout_clean.tmp cout.tmp')
    
    



