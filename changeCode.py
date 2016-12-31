import codecs
import os  
import random

SOURCE_ROOT_DIR = 'demoNews/TSMC/'
TARGET_ROOT_DIR = 'UTF8NEWS/'
BLOCKSIZE = 1048576 # or some other, desired size in bytes

if not os.path.exists(TARGET_ROOT_DIR):
    os.makedirs(TARGET_ROOT_DIR)
for fileName in os.listdir(SOURCE_ROOT_DIR):
     #if os.path.isfile(fn):

	sourceFileName = SOURCE_ROOT_DIR + fileName
	targetFileName = TARGET_ROOT_DIR + str(random.randint(1, 1000000)) + ".txt"
	print(targetFileName)
	with codecs.open(sourceFileName, "r", "gb18030") as sourceFile:
	    with codecs.open(targetFileName, "w+", "utf-8") as targetFile:
	        while True:
	            contents = sourceFile.read(BLOCKSIZE)
	            if not contents:
	                break
	            targetFile.write(contents + '\n')

# BLOCKSIZE = 1048576 # or some other, desired size in bytes
# with codecs.open(sourceFileName, "r", "gb18030") as sourceFile:
#     with codecs.open(targetFileName, "w", "utf-8") as targetFile:
#         while True:
#             contents = sourceFile.read(BLOCKSIZE)
#             if not contents:
#                 break
#             targetFile.write(contents)