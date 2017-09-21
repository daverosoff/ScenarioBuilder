from __future__ import print_function

import os
import re
import shutil
import tempfile
import subprocess
import argparse

def newscen():
    tmpdir = tempfile.mkdtemp(prefix="newscen-", dir=".")
    shutil.copy("P01_windturbine_original.Rnw", tmpdir)
    # shutil.copy("sb.R", tmpdir)
    os.chdir(tmpdir)
    subprocess.call(["R", "CMD", "Sweave", "P01_windturbine_original.Rnw"])
    with open("group_id.txt", 'rw') as idfile:
        group_id = idfile.readline()
    idfile.close()
    newRnw = group_id + ".Rnw"
    newsoltex = group_id + "-solution.tex"
    newsolpdf = group_id + "-solution.pdf"
    # csv = "windfarm-" + group_id + ".csv"
    newf = [newRnw, newsoltex, newsolpdf]
    shutil.move("P01_windturbine_original.Rnw", newRnw)
    shutil.move("P01_windturbine_original.tex", newsoltex)
    subprocess.call(["pdflatex", "--interaction=nonstopmode", newsoltex])
    os.chdir("..")
    os.mkdir(group_id)
    for f in newf:
        shutil.copy2(tmpdir + "/" + f, group_id)
    # shutil.copy2(tmpdir + "/" + csv, group_id)
    os.chdir(group_id)
    redactedtex = re.sub("-solution.tex", ".tex", newsoltex)
    redactedpdf = re.sub("tex$", "pdf", redactedtex)
    sedcall = '/BEGIN ANSWER KEY SECTION/,/END ANSWER KEY SECTION/d'
    with open(redactedtex, 'w') as target:
        subprocess.call(["sed", "-e", sedcall, newsoltex], stdout=target)
    shutil.rmtree("../" + tmpdir)
    tmpdir = tempfile.mkdtemp(prefix="redacted-", dir=".")
    print("Moving", redactedtex, "to", tmpdir)
    shutil.move(redactedtex, tmpdir)
    os.chdir(tmpdir)
    subprocess.call(["pdflatex", "--interaction=nonstopmode", redactedtex])
    shutil.move(redactedtex, "../" + redactedtex)
    shutil.move(redactedpdf, "../" + redactedpdf)
    os.chdir("..")
    os.mkdir("key")
    for file in newf:
        shutil.move(file, "key")
    try:
        shutil.rmtree(tmpdir)
    except e:
        print(e)
    os.chdir("..")

parser = argparse.ArgumentParser(description="Generate new scenario and key.")
parser.add_argument('--num', default=1, help="Number of scenarios to generate")
args = parser.parse_args()

for i in range(int(args.num)):
    newscen()

