"""Make the CV PDF that the 'erica' link in index.html opens.

Keep this file, RES100726ML.docx and index.html in the same folder.

    python make_cv_pdf.py            convert once
    python make_cv_pdf.py --watch    convert again every time the .docx is saved (Ctrl+C to stop)

Writes RES100726ML.docx.pdf next to the .docx. Uses Microsoft Word if it is installed
(Windows), otherwise LibreOffice. Standard library only, nothing to pip install.
"""
import os
import shutil
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
DOCX = os.path.join(HERE, "RES100726ML.docx")
PDF = DOCX + ".pdf"


def via_word(docx, pdf):
    """Windows + Microsoft Word, driven through PowerShell (no Python packages needed)."""
    if os.name != "nt" or not shutil.which("powershell"):
        return False
    q = lambda p: "'" + p.replace("'", "''") + "'"          # PowerShell single-quoted string
    script = (
        "$w = New-Object -ComObject Word.Application; $w.Visible = $false; "
        "try { $d = $w.Documents.Open(%s, $false, $true); "
        "$d.ExportAsFixedFormat(%s, 17); $d.Close($false) } finally { $w.Quit() }"
    ) % (q(docx), q(pdf))
    r = subprocess.run(["powershell", "-NoProfile", "-Command", script],
                       capture_output=True, text=True)
    return r.returncode == 0 and os.path.exists(pdf)


def via_libreoffice(docx, pdf):
    candidates = [shutil.which("soffice"), shutil.which("libreoffice"),
                  r"C:\Program Files\LibreOffice\program\soffice.exe",
                  r"C:\Program Files (x86)\LibreOffice\program\soffice.exe"]
    exe = next((c for c in candidates if c and os.path.exists(c)), None)
    if not exe:
        return False
    with tempfile.TemporaryDirectory() as tmp:
        r = subprocess.run([exe, "--headless", "--convert-to", "pdf", "--outdir", tmp, docx],
                           capture_output=True, text=True)
        made = os.path.join(tmp, os.path.splitext(os.path.basename(docx))[0] + ".pdf")
        if r.returncode != 0 or not os.path.exists(made):
            return False
        shutil.move(made, pdf)
    return True


def convert():
    if not os.path.exists(DOCX):
        sys.exit(f"Not found: {DOCX}")
    with tempfile.TemporaryDirectory(dir=HERE) as tmp:       # build aside, then swap in
        new_pdf = os.path.join(tmp, "cv.pdf")
        ok = via_word(DOCX, new_pdf) or via_libreoffice(DOCX, new_pdf)
        if not ok:
            sys.exit("Could not convert. Install Microsoft Word or LibreOffice, "
                     "or export the PDF yourself and save it as " + os.path.basename(PDF))
        try:
            os.replace(new_pdf, PDF)
        except PermissionError:
            sys.exit("Close " + os.path.basename(PDF) + " (it is open somewhere) and try again.")
    print(time.strftime("%H:%M:%S"), "wrote", os.path.basename(PDF))


if __name__ == "__main__":
    convert()
    if "--watch" in sys.argv:
        print("Watching", os.path.basename(DOCX), "- Ctrl+C to stop")
        last = os.path.getmtime(DOCX)
        try:
            while True:
                time.sleep(2)
                m = os.path.getmtime(DOCX)
                if m != last:
                    time.sleep(1.5)                          # let the save finish
                    last = os.path.getmtime(DOCX)
                    convert()
        except KeyboardInterrupt:
            pass
