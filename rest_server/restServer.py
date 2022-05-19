#and so the server begins for real
#whatever I need to implement key value store
#endpoint: /kvs/<key>
import json
import sys
import flask
import os
import requests
from requests.exceptions import ConnectionError
from flask import request

app = flask.Flask(__name__)
app.config["DEBUG"] = False

#sys.argv len of argv for argc
#also getopt.getopt(args, options, [long_options])
# timeout after 5seconds

store = {
"test": "entry"
}
print (sys.argv[0])
#print (sys.argv[1])
try: 
    forward = os.environ['FORWARDING_ADDRESS']
except KeyError:  
    forward = '' #now we have forwarding address
#FORWARDING_ADDRESS == '' is main
#else is follower and forward request 

@app.route('/hello', methods=['GET']) #/hello
def hello():
    return store["test"];

@app.route('/hello/<name>', methods=['POST']) #/hello/<name>
def nameHandle(name):
    return "Hello, " + name + "!"

@app.route('/echo/<msg>', methods=['GET', 'POST'])  #/echo/<msg>
def msgHandle(msg):
	if request.method == 'POST':
		return "POST message received: " + msg
	else:
		return "this method is unsupported.", 405
	

@app.route('/kvs/<key>', methods=['PUT', 'GET', 'DELETE'])
def samepleHandle(key):
	if request.method == 'PUT':
		if not forward == "":
			destination = "http://" + forward + "/kvs/" + key
			try:
				sentToMain = requests.put(destination, data =request.data , timeout =4)
			except ConnectionError as err:
				print(err)
				return {"error":"Main instance is down","message":"Error in PUT"}, 503
			print (sentToMain.status_code )
			if sentToMain.status_code == 500:#issue with getting a response from main
				return {"error":"Main instance is down","message":"Error in PUT"}, 503
			else:
				return sentToMain.content, sentToMain.status_code
		if key == '':
			return {"error":"Value is missing","message":"Error in PUT"}, 400
		if len(key) > 50:
			return {"error":"Key is too long","message":"Error in PUT"}, 400
		if key in store:
			if json.loads(request.data) == {}:
				return {"error":"Value is missing","message":"Error in PUT"}, 400

			#print(request.data)
			keyVal= json.loads(request.data)
			val = keyVal['value']
			#print(keyVal['value']) # we can get the data now what do we do with it
			#print(request.values)
			store[key] = val
			return {"message":"Updated successfully","replaced":True}
		if not key in store:
			#print(request.args)
			if json.loads(request.data) == {}:
				return {"error":"Value is missing","message":"Error in PUT"}, 400

			#print(request.data)
			keyVal= json.loads(request.data)
			val = keyVal['value']
			#print(keyVal['value']) # we can get the data now what do we do with it
			#print(request.values)
			store[key] = val
			return {"message":"Added successfully","replaced":False}, 201
		return "no ifs tripped put"
		
		
		
	elif request.method == 'GET':
		if not forward == "":
			destination = "http://" + forward + "/kvs/" + key
			try:
				sentToMain = requests.get(destination, data =request.data , timeout =4)
			except ConnectionError as err:
				print(err)
				return {"error":"Main instance is down","message":"Error in GET"}, 503
			print (sentToMain.status_code )
			if sentToMain.status_code == 500:#issue with getting a response from main
				return {"error":"Main instance is down","message":"Error in GET"}, 503
			else:
				#print (sentToMain.content)
				return sentToMain.content
		if not key in store:
			return {"doesExist":False,"error":"Key does not exist","message":"Error in GET"}, 404
		elif key in store:
			print (store)
			print ({"doesExist":True,"message":"Retrieved successfully", key:store[key]})
			return {"doesExist":True,"message":"Retrieved successfully", "value":store[key]}#200
	
	
	
	elif request.method == 'DELETE':
		if not forward == "":
			destination = "http://" + forward + "/kvs/" + key
			try:
				sentToMain = requests.delete(destination, data =request.data , timeout =4)
			except ConnectionError as err:
				print(err)
				return {"error":"Main instance is down","message":"Error in DELETE"}, 503
			print (sentToMain.status_code )
			if sentToMain.status_code == 500:#issue with getting a response from main
				return {"error":"Main instance is down","message":"Error in DELETE"}, 503
			else:
				return sentToMain.content, sentToMain.status_code
		if not key in store:
			return {"doesExist":False,"error":"Key does not exist","message":"Error in DELETE"}, 404
		elif key in store:
			store.pop(key)
			return {"doesExist":True,"message":"Deleted successfully"} #200
	


app.run (host = '0.0.0.0', port = 13800)
#http://0.0.0.0:13800/
#http://127.0.0.1:13800/

"""
curl --request   PUT                               \
           --header    "Content-Type: application/json"  \
           --write-out "%{http_code}\n"                  \
           --data      '{"value": "sampleValue"}'        \
           http://127.0.0.1:13800/kvs/sampleKey
"""