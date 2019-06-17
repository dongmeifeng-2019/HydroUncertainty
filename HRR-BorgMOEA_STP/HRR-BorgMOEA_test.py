from sys import *
from math import *
from borg import *
import string, os
import shutil

nvars = 6
nobjs = 5
pos = [10,11,67,68,70,71]

#os.system(r"C:\Research\HRR-BorgMOEA\STP_S1SS1.exe")

def HRRBorg(*vars):
    i = 1
    k = 0
    #initialize the parameters in HRR
    with open('input_num1.txt') as old, open('input_num.txt', 'w') as new:
        for line in old:
            if k<nvars:
                if i==pos[k]:
                    #print line.split(' ', 1)[1].strip()
                    #new.seek(0)
                    new.write(str(vars[k])+' '+line.split(' ', 1)[1].strip()+'\n')
                    k = k+1
                else:
                    new.write(line)
            else:
                new.write(line)
            
            #print line.split(' ', 1)[1].strip()
            i = i + 1
            
    shutil.move('input_num.txt', 'input_num1.txt')
    shutil.copy('input_num1.txt', 'input_num.txt')

    #call HRR
    os.system(r"C:\Research\HRR-BorgMOEA\STP_S1SS1.exe")
    #extract error metrics from HRR output and return to objs
    with open('bestpara_00100_00_00_1984.txt') as f1:
        obj = f1.readlines()

    #store the results
    with open('cal_metrics.txt','a') as f2:
        f2.write(obj[1])
    f2.close()

    objs = [1.0]*nobjs
    ####
    for i in range(nobjs):
        objs[i] =1 - float(obj[1].split()[nvars-1+4*i])    ###1-NSE
    #print objs
    f1.close()
    os.remove('bestpara_00100_00_00_1984.txt')
    return objs
    
##
borg = Borg(nvars, nobjs, 0, HRRBorg)
borg.setBounds([10,200],[1,20],[0.001,5],[0.001,5],[0.01,20],[-10,0])
borg.setEpsilons(*[0.001]*nobjs)
##
result = borg.solve({"maxEvaluations":1000})
##
##for solution in result:
##	print(solution.getObjectives())

