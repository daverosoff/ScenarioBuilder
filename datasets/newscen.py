#/usr/bin/python

import os
import re
import shutil
import tempfile
import subprocess
import argparse

def newscen():
    tmpdir = tempfile.mkdtemp(prefix="newscen-", dir=".")
    shutil.copy("dataset-gen.Rnw", tmpdir)
    os.chdir(tmpdir)
    newRnw = group_id + ".Rnw"
    newsoltex = group_id + "-solution.tex"
    newsolpdf = group_id + "-solution.pdf"
    newf = [newRnw, newsoltex, newsolpdf]
    shutil.move("dataset-gen.Rnw", newRnw)
    subprocess.call(["R", "CMD", "Sweave", newRnw])
    subprocess.call(["pdflatex", "--interaction=nonstopmode", newsoltex])
    with open("group_id.txt", 'r') as idfile:
        group_id = idfile.readline()
    idfile.close()
    os.chdir("..")
    os.mkdir(group_id)
    for f in newf:
        shutil.copy2(tmpdir + "/" + f, group_id)
    os.chdir(group_id)
    redactedtex = re.sub("-solution.tex", "", newsoltex)
    redactedpdf = re.sub("tex$", "pdf", redactedtex)
    sedcall = r"'/BEGIN ANSWER KEY SECTION/,/END ANSWER KEY SECTION/d'"
    subprocess.call(["sed", "-e", sedcall, newsoltex, ">", redactedtex])
    tmpdir = tempfile.mkdtemp(prefix="redacted-", dir=".")
    shutil.move(redactedtex, tmpdir)
    os.chdir(tmpdir)
    subprocess.call(["pdflatex", "--interaction=nonstopmode", redactedtex])
    shutil.move(redactedtex, "..")
    shutil.move(redactedpdf, "..")
    os.chdir("..")
    os.mkdir("key")
    for file in newf:
        shutil.move(file, "key")

if __NAME__ == "__MAIN__":
    parser = argparse.ArgumentParser(description="Generate new scenario and key.")
    parser.add_argument('--num', default=1, help="Number of scenarios to generate")
    args = parser.parse_args()

    for i in range(args.num):
        newscen()
