"""Load the 11 downloaded Yellowbrick datasets into MySQL database `Yb-11`.

Prereqs:
  pip install pandas sqlalchemy pymysql yellowbrick
  python -m yellowbrick.download        (already done)

Run:
  python load_yb_to_mysql.py --password YOURPW
"""
import argparse

import pandas as pd
from sqlalchemy import create_engine, text
from sqlalchemy.dialects.mysql import LONGTEXT
from sqlalchemy.engine import URL

import yellowbrick.datasets as yb

NAMES = ["bikeshare", "concrete", "credit", "energy", "game", "hobbies",
         "mushroom", "nfl", "occupancy", "spam", "walking"]

p = argparse.ArgumentParser()
p.add_argument("--host", default="localhost")
p.add_argument("--port", type=int, default=3306)
p.add_argument("--user", default="root")
p.add_argument("--password", default="")
p.add_argument("--db", default="Yb-11")
a = p.parse_args()


def url(db=None):
    return URL.create("mysql+pymysql", a.user, a.password, a.host, a.port, db)


with create_engine(url()).begin() as c:
    c.execute(text(f"CREATE DATABASE IF NOT EXISTS `{a.db}` CHARACTER SET utf8mb4"))

engine = create_engine(url(a.db))


def to_frame(name):
    loader = getattr(yb, f"load_{name}")
    if name == "hobbies":  # text corpus, not X,y frames
        corpus = loader()
        return pd.DataFrame({"text": list(corpus.data), "target": list(corpus.target)})
    X, y = loader()
    df = pd.DataFrame(X).copy()
    if y is not None:
        df["target"] = list(y)
    return df


ok, failed = [], []
for n in NAMES:
    try:
        df = to_frame(n)
        df.columns = [str(c) for c in df.columns]
        dtype = {"text": LONGTEXT()} if n == "hobbies" else None
        df.to_sql(f"yb_{n}", engine, if_exists="replace", index=False,
                  chunksize=5000, dtype=dtype)
        print(f"yb_{n}: {len(df)} rows x {df.shape[1]} cols")
        ok.append(n)
    except Exception as e:
        print(f"FAILED {n}: {e}")
        failed.append(n)

print(f"\ndone -> `{a.db}`  loaded {len(ok)}/{len(NAMES)}"
      + (f"  failed: {failed}" if failed else ""))
