import re
inn=open('hsh.com.txt').read()

def d2i(x):
 arr=x.split('-')
 month='?'
 if arr[0]=='Jan':
  month='01'
 if arr[0]=='Feb':
  month='02'
 if arr[0]=='Mar':
  month='03'
 if arr[0]=='Apr':
  month='04'
 if arr[0]=='May':
  month='05'
 if arr[0]=='Jun':
  month='06'
 if arr[0]=='Jul':
  month='07'
 if arr[0]=='Aug':
  month='08'
 if arr[0]=='Sep':
  month='09'
 if arr[0]=='Oct':
  month='10'
 if arr[0]=='Nov':
  month='11'
 if arr[0]=='Dec':
  month='12'

 return(arr[-1]+'-'+month)

arr=inn.split('<div class="Row">')
for x in arr:
 y=re.findall('(?<=>)[^<]+',x)
 print d2i(y[0])+','+str(float(y[-1][0:-1])/100)

