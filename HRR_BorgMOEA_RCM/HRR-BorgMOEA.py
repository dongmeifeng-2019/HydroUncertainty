from sys import *
from math import *
from borg import *
import string, os
import shutil

os. chdir("C:\Research\HRR_BorgMOEA_RCM")    #working directory
nvars = 6           #number of parameters for calibration
nobjs = 5           #number of objectives
pos = [10,11,13,40,41,42] #parameter position (line number) in input.txt
error_file = 'bestpara_00100_00_00_1984.txt'   #name of file containing error metrics

def HRRBorg(*vars):
    i = 1
    k = 0
    exists = os.path.isfile(error_file)
    if exists:
        os.remove(error_file)
        # Keep presets
    #initialize the parameters in HRR
    with open('input_num1.txt') as old, open('input_num.txt', 'w') as new:
        for line in old:
            if k<nvars:
                if i==pos[k]:
                    print line.split(' ', 1)[1].strip()
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

    #call HRR executable, change the name accordingly
    os.system("run.exe")
    #extract error metrics from HRR output and return to objs
    with open(error_file) as f1:
        obj = f1.readlines()

    #store the results
    with open('cal_metrics.txt','a') as f2:
        f2.write(obj[1])
    f2.close()

    objs = [1.0]*nobjs
    ####
    for i in range(nobjs):
        #change here based on the position of needed objectives in the error_file
        objs[i] =1 - float(obj[1].split()[nvars-1+4*i])    ###1-NSE
    #print objs
    f1.close()
    os.remove(error_file)
    return objs
    
##
borg = Borg(nvars, nobjs, 0, HRRBorg)
#set the parameter boundaries
borg.setBounds([10,200],[1,20],[0.01,5.0],[0.00,0.3],[0.2,0.8],[0.2,0.8])
#set the stop criteria
borg.setEpsilons(*[0.001]*nobjs)
##
result = borg.solve({"maxEvaluations":1000})
