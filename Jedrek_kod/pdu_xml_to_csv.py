# the original XML files were converted to CSV by Marek Gagolewski
# using the following code (C) 2017 gagolewski.com
import xml.etree.ElementTree as ET
import pandas as pd


def xml2csv(fname, forum, delcols=[]):
    tree = ET.parse(fname)
    root = tree.getroot()
    d = pd.DataFrame([e.attrib for e in root])
    for name in delcols: del d[name]
    fname = fname.split("/")[1]

    d.to_csv(f"{forum}_csv/" + fname + ".csv", index=False)


fora = ["fitness", "sport", "gaming"]
for forum in fora:
    xml2csv(f"{forum}_xml/Badges.xml", forum)
    xml2csv(f"{forum}_xml/PostLinks.xml", forum)
    xml2csv(f"{forum}_xml/PostHistory.xml", forum)
    xml2csv(f"{forum}_xml/Posts.xml", forum, ["Body", "OwnerDisplayName", "LastEditorDisplayName"])
    xml2csv(f"{forum}_xml/Tags.xml", forum)
    xml2csv(f"{forum}_xml/Users.xml", forum, ["AboutMe", "WebsiteUrl", "ProfileImageUrl"])
    xml2csv(f"{forum}_xml/Votes.xml", forum)
    xml2csv(f"{forum}_xml/Comments.xml", forum, ["Text", "UserDisplayName"])
