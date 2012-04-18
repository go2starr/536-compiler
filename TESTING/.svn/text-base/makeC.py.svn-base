from os import system

fd = open('filenames')
s = fd.read()
for i in s.split('\n'):
    y =  i.find('.Little') 
    if y != -1: 
        system('cp ' + i + ' ' + i[:y] + '.c')
