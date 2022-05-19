
FROM python:3.8
#RUN apk add update
#RUN apk add python:3.7-slim

	
WORKDIR /app
COPY . /app
RUN pip3 install flask # find out what is needed here probably flask and that is it
RUN pip3 install requests
EXPOSE 13800

ENTRYPOINT ["python3"]

CMD ["restServer.py"]