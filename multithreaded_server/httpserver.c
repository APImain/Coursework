#include <sys/socket.h>
#include <sys/stat.h>
#include <stdio.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <fcntl.h>
#include <unistd.h> // write
#include <string.h> // memset
#include <stdlib.h> // atoi
#include <stdbool.h> // true, false
#include <errno.h>
#include <pthread.h>

#define BUFFER_SIZE 4096

//#define NUMTHREADS 2

//int log_ready;
int global_offset;
pthread_cond_t full_var = PTHREAD_COND_INITIALIZER;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2= PTHREAD_MUTEX_INITIALIZER;
//int thread_id[NUMTHREADS]  = {0, 1};


struct httpObject {
    /*
        Create some object 'struct' to keep track of all
        the components related to a HTTP message
        NOTE: There may be more member variables you would want to add
    */
    char method[12];         // PUT, HEAD, GET / list of commands we can use
    char filename[100];      // what is the file we are worried about /name
    char httpversion[9];    // HTTP/1.1 /shout stay constant
    long int content_length; // example: 13 /size of content
    int status_code;
    char buffer[BUFFER_SIZE];
};

struct worker{
	int id;
	ssize_t client_socket;
	struct httpObject request;
	char* log_name;
	pthread_t worker_id;
	pthread_cond_t condition_variable;
	pthread_mutex_t* lock;
};

struct dispatcherInfo{
	//filename, socket, thread count
	char* filename;
	char socket[6];
	int thread_count;
};



void health_check(char * log_name){
		char buffer[BUFFER_SIZE];
		int entries = 0;
		int errors = 0;
		char s_entries[20];
		char* healthcheck = "healthcheck";
		//char* s_errors;
		//check for the total number of ======
		// count the number of FAIL: there are 
		//does its own format of request I think or can just use frame work already set out I dont know
		int fd = open(log_name, O_RDONLY );
		char substr[7];

		int fd_health = open(healthcheck , O_RDWR | O_CREAT , 00777); // open or create if not open
		if (fd <0) {
			
			sprintf(s_entries, "%d\n%d", entries, errors);
			int write_data = write(fd_health,  s_entries, strlen(s_entries)); //write to special health file
			if (write_data < 0)
				warn("%s", fd_health);
			return;
		}
		int read_data = read(fd, buffer , BUFFER_SIZE);
		//printf("buffer size %d\n", read_data);
		//printf("buffer size %ld\n", strlen(buffer) );
		//printf("error:%s\n", buffer);
		for (unsigned int i = 0; i < (read_data)-5 ; i++){
			memcpy( substr, &buffer[i], 6 );
			substr[6] = '\0';
			if(strcmp(substr, "FAIL: ")==0){
				errors++;
			}
			if (strcmp(substr, "======")==0){
				entries++;
			}
			//printf("%s-", substr);
		}
		//loop to check buffer
		//search for ====== and FAIL
		while (read_data == BUFFER_SIZE){
			
			read_data = read(fd, buffer , BUFFER_SIZE);

			for (unsigned int i = 0; i < (read_data)-5 ; i++){
				memcpy( substr, &buffer[i], 6 );
				substr[6] = '\0';
				if(strcmp(substr, "FAIL: " )==0){
					errors++;
				}
				if (strcmp(substr, "======" )==0){
					entries++;
				}
							//printf("%s-", substr);
			}

		}
		//write errors 
		entries = entries / 3;
		sprintf(s_entries, "%d\n%d", errors, entries);
		printf ("wrote:\n%d\n%d", errors, entries);
		int write_data = write(fd_health,  s_entries, strlen(s_entries)); //write to special health file
		if (write_data < 0)
				warn("%s", fd_health);
		close(fd);
		close(fd_health);
}

void make_hex(char* input, char * out ,int size, int prev){
	//char output[size*4] ;
	char temp[20] = "";
	char  head[9] = ""; //= "line number"
	char * tail = "\n";
	int added_line = 0;
	//parse from string using size as delimiter
	//insert relevant data into the format 
	//for(){}
	strcpy (out, "");
	for (int i = 0; i < size; i++){
		added_line = 0;
		if ( (i +prev)  % 20 == 0 ){
			//sprintf (temp, "line number");
			sprintf(head , "%08d", i + prev);
			strcat(out, head);
			//printf("head: %s\n", head);
		}
		sprintf(temp, " %02x", ((unsigned char)input[i] ));
		//printf("mid: %s\n", temp);
		strcat(out, temp);  // input[i] + " "
		if ( (i+prev)  % 20 == 19 ){
			//sprintf(temp, "\n");
			added_line = 1;
			strcat(out,  tail );
			//printf("tail: %s\n", tail);
		}
	}
	if ( added_line ==0 && size < 4096 ){
			//sprintf(temp, "\n");
			added_line = 1;
			strcat(out,  tail );
			//printf("tail: %s\n", tail);
	}
	//printf("output: %s" , out);
	//memcpy(out, output, strlen(output)+1);
	//return output;//output;
}

int total_bytes(struct httpObject* message){
	int total = 0;
	int header = 0; 
	int line_chars = 9 * ((message->content_length + 19)/20);
	if (message->status_code== 200 || message->status_code==201){//not fail cases
		char len_chars [20]= "";
		sprintf(len_chars, "%ld" , message->content_length);
		header = 11 + strlen(message->method)+ strlen(message->filename) + strlen(len_chars); //add content length to end
	}
	else{
		//fail case
		header = 24 + strlen(message->method) + strlen(message->filename) + strlen(message->httpversion) + 3; // plus 3 for code
	}
	total = header + (message->content_length*3) + line_chars + 9 ; // end =s randomly added 13
	if (strcmp(message->method, "HEAD" ) ==0){
		total = header +9;
	}
	printf ("total bytes: %d\n", total  );
	return total;
}

/*
    \brief 1. Want to read in the HTTP message/ data coming in from socket
    \param client_sockd - socket file descriptor
    \param message - object we want to 'fill in' as we read in the HTTP message
*/
void read_http_response(ssize_t client_sockd, struct httpObject* message) {
    printf("This function will take care of reading message\n");
    int request = recv(client_sockd, message->buffer, BUFFER_SIZE, 0);
    if (request == -1) {
	warn("%s", client_sockd);//this might be an example of a 500 error
	message->status_code = 500;
	message -> content_length = 0;
	return;
    }
    //printf("%s\n", message->buffer);
	char new[234];
    sscanf (message->buffer, "%[^' /'] /%s %[^'\r\n']", message->method, message->filename, message->httpversion); // %[^','],%[^','],%[^','],%s, );
    if (strcmp(message->method, "PUT")==0){
		//printf("%s\n", message->buffer);
	int scan =sscanf (message->buffer, "%[^' /'] /%s %[^'\r\n']\r\n%[^'C']Content-Length: %ld", message->method, message->filename, message->httpversion, new, &message->content_length); //[^'\r\n']
	    //printf("%dmessage printed: %s| %s| %s| %ld| %d\n", scan, message->method, message->filename, message->httpversion, message->content_length, message->status_code);
	message->status_code = 200;
    }
    else if (strcmp(message->method, "GET")==0){
	message->status_code = 200;
    }
    else if (strcmp(message->method, "HEAD")==0){
	message->status_code = 200;
    }
    else {
	message->status_code = 400;
	message -> content_length = 0;
    }
	//printf("what is after %s\n", new);
    //printf("message printed: %s| %s| %s| %ld| %d\n", message->method, message->filename, message->httpversion, message->content_length, message->status_code);
    //check if file name is bad
    for (unsigned int i = 0; i<strlen(message->filename); i++){
	char filec = (message->filename[i]);
	bool validc = ((filec >= 'a') && (filec <= 'z')) || 
            ((filec >= 'A') && (filec <= 'Z')) || 
            ((filec >= '0') && (filec <= '9')) || 
	    (filec == '-') ||
	    (filec == '_');	
	if (!validc){
		message->status_code = 400;
		message->content_length = 0;
		//printf("400 error detected\n");     
	}
    }
	if (strlen(message->filename) > 27){
		message->status_code = 400;
		message->content_length = 0;
	}
	if (strcmp(message->httpversion, "HTTP/1.1")!=0){
		message->status_code = 400;
		message->content_length = 0;
	}
    /*
     * Start constructing HTTP request based off data from socket
	* this is the parse thing where we take the command and break it down
	*what happens if client_sockd is 4096 then what
	*only letters numbers underscore and dash
     */
	 //sadfkjfjeejsdfk
    printf("done read\n");
    return;
}

/*
    \brief 2. Want to process the message we just recieved
*/
void process_request(ssize_t client_sockd, struct httpObject* message,struct worker* thread) {
    printf("Processing Request\n");
    //catch bad file name
    if(message->status_code != 200){
	message->content_length = 0;
	return;
    }
    if(strcmp(message->method, "PUT")==0){
    	//get file and put on server not done doesnt work with perms
	if (strcmp(message->filename, "healthcheck" )==0 ){ //protect health check
		//warn("%s", message->filename); 
		message->status_code = 403;//if file not found
		message->content_length = 0;
		return;
	}
	int fd = open(message->filename, O_RDWR); // open file

	if (fd == -1){  
		if (errno == 2){
			fd = open(message->filename, O_RDWR | O_CREAT, 00777);//if file not found
		}
		else if (errno == 13){
			warn("%s", message->filename); 
			message->status_code = 403;//if file not found
			message->content_length = 0;
			return;
		}
		else{
			warn("%s", message->filename); 
			message->status_code = 500;
			message->content_length = 0;
			return;
		}
	}
	//check if writing is allowed here if we create will be but if not issues this whole section might need to be moved to construct
	close(fd);
	message->status_code = 201;
	//we can calc the size of the space we need here
    }

    if(strcmp(message->method, "GET")==0){
    	//look for file
	//check for permission
	if (strcmp(message->filename, "healthcheck" )==0 && thread->log_name != NULL){
		health_check(thread->log_name);
		printf("calculated healthcheck\n");
		int fd = open(message->filename, O_RDONLY);
		struct stat sb; // make sure correct content length as file gets modified here
		fstat(fd, &sb);
		message->content_length = sb.st_size;
		close(fd);
	}
	int fd = open(message->filename, O_RDONLY); // open file
	if (fd == -1){  
		warn("%s", message->filename); 
		//printf("%d\n", errno);
		if (errno == 2){
			message->status_code = 404;//if file not found
			message->content_length = 0;
		}
		else if (errno == 13){
			message->status_code = 403;//if bad permissions
			message->content_length = 0;
		}
		else{
			message->status_code = 500; //some other error
			message->content_length = 0;
		}
	}
	else{
		struct stat sb;
		fstat(fd, &sb);
		message->content_length = sb.st_size;
		close(fd);
	}
    }

    if(strcmp(message->method, "HEAD")==0){
	if (strcmp(message->filename, "healthcheck" )==0 ){
		//warn("%s", message->filename); 
		message->status_code = 403;//if file not found
		message->content_length = 0;
		return;
	}
	int fd = open(message->filename, O_RDONLY);
    	if (fd == -1){  
		warn("%s", message->filename); 
		if (errno == 2){
			message->status_code = 404;//if file not found
			message->content_length = 0;
		}
		else if (errno == 13){
			message->status_code = 403;//if bad permissions
			message->content_length = 0;
		}
		else{
			message->status_code = 500; //some other error
			message->content_length = 0;
		}
	}
	else{
		struct stat sb;
		fstat(fd, &sb);
		message->content_length = sb.st_size;
		close(fd);
	}
    }
    //printf("message printed: %s| %s| %s| %ld| %d\n", message->method, message->filename, message->httpversion, message->content_length, message->status_code);
    return;
}

/*
    \brief 3. Construct some response based on the HTTP request you recieved
*/
void construct_http_response(ssize_t client_sockd, struct httpObject* message, struct worker* thread) {
    printf("Constructing Response\n");
	char hex_str[BUFFER_SIZE * 4];
	int log_entry_size;
	int line_number = 0;
	//printf("message printed: %s| %s| %s| %ld| %d\n", message->method, message->filename, message->httpversion, message->content_length, message->status_code);
    char response[BUFFER_SIZE];
	char log_buffer[BUFFER_SIZE * 3];
    char *code_text = malloc(30);
	//char* hex_convert;
    //
	int local_offset;
	//calc the value of the offset
	int calc_offset = total_bytes(message);
	//wait
	//while(false){//modify
	//wait
		//printf("waiting 2\n");
		//pthread_cond_wait(&log_var, thread->lock);

	
		// startcrit
	if(thread->log_name != NULL){
		pthread_mutex_lock( &mutex1 );
		local_offset = global_offset;
		//modify global offset
		global_offset = global_offset + calc_offset;
		//
		pthread_mutex_unlock( &mutex1);
		printf("signal done\n");
	}
	//pthread_cond_signal(&log_var);
	//end crit
	//get text for codes
    switch (message->status_code)
    {
	    case 200: strcpy(code_text, "OK");
		break;
	    case 201: strcpy(code_text, "Created");
		break;
	    case 400: strcpy(code_text, "Bad Request");
		break;
	    case 403: strcpy(code_text, "Forbidden");
		break;
	    case 404: strcpy(code_text, "Not Found");
    	        break;
	    default: strcpy(code_text, "Internal Server Error");
    }
	
	
	//critical rejoin for changing global offset var
	//everything below here is non critical
	int log_fd = open(thread->log_name, O_RDWR);
    //handles successful get ouput sends header with file size and sends file as well
    if(message->status_code ==200 && strcmp(message->method, "GET")==0){
    	//first build header
	snprintf (response, sizeof(response), "%s %d %s\r\nContent-Length: %ld\r\n\r\n", message->httpversion, message->status_code, code_text, message->content_length);
	send(client_sockd, response, strlen(response), 0);
	
	//handle log file
	if (thread->log_name != NULL){
		log_entry_size =  sprintf(log_buffer, "%s /%s length %ld\n", message->method, message->filename, message->content_length); 
		pwrite(log_fd, log_buffer, log_entry_size, local_offset);
		local_offset = log_entry_size + local_offset;
	}
	
	//then start returning file to a client after all checks are run

	int fd = open(message->filename, O_RDONLY); //open has already been checked in a prior message
	int read_data = read(fd, message->buffer, BUFFER_SIZE);
	if(read_data == -1){
		warn("%s", message->filename);
		return;
	}
	int send_data = send(client_sockd, message->buffer, read_data, 0);
	if(send_data == -1){
		warn("%s", client_sockd);
		return;
	}	
	//printf("sent to client in theory: %s\n", message->buffer);
		if (log_fd > 0){
			make_hex(message->buffer, hex_str,read_data, line_number); //hex_convert = 
			log_entry_size =  sprintf(log_buffer, "%s", hex_str);//(char*)make_hex(message->buffer, recv_data));
			pwrite(log_fd, log_buffer, log_entry_size, local_offset);
			local_offset = log_entry_size + local_offset;
		}
	while(read_data == BUFFER_SIZE){
		line_number = read_data + line_number;
	    read_data = read(fd, message->buffer, BUFFER_SIZE);
	    if(read_data == -1){
		warn("%s", message->filename);
		return;
	    }
	    send_data = send(client_sockd, message->buffer, read_data, 0);
	    if(send_data == -1){
		warn("%s", client_sockd);
		return;
	    }	
		
		if (log_fd > 0){
			//printf("cycle\n");
			make_hex(message->buffer, hex_str,read_data, line_number);
			log_entry_size =  sprintf(log_buffer, "%s", hex_str);//(char*)make_hex(message->buffer, recv_data));
			pwrite(log_fd, log_buffer, log_entry_size, local_offset);
			local_offset = log_entry_size + local_offset;
		
		}
		
		
	    //loop to return big data
	}
	if (thread->log_name != NULL){
		printf("arriving here?\n" );
		log_entry_size =  sprintf(log_buffer, "========\n"); 
		pwrite(log_fd, log_buffer, log_entry_size, local_offset);
		local_offset = log_entry_size + local_offset;
	}
	remove("healthcheck");//delete temp file
	close(fd);
    }
    //handles successful put operation generates response it should have already done the requested tax
    else if(strcmp(message->method, "PUT")==0 && message->status_code ==201){
		snprintf (response, sizeof(response), "%s %d %s\r\nContent-Length: 0\r\n\r\n", message->httpversion, message->status_code, code_text);
		if (message->content_length == 0 ){
			send(client_sockd, response, strlen(response), 0);
			return;
		}
		
		if (thread->log_name != NULL){
			log_entry_size =  sprintf(log_buffer, "%s /%s length %ld\n", message->method, message->filename, message->content_length); 
			pwrite(log_fd, log_buffer, log_entry_size, local_offset);
			local_offset = log_entry_size + local_offset;
		}
		//printf("%s\n", response);
			//check if writing is allowed here if we create will be but if not issues this whole section might need to be moved to construct
		int fd = open(message->filename, O_RDWR);
		
		int recv_data = recv(client_sockd, message->buffer, BUFFER_SIZE, 0);
		//printf("should be first inpput%s\n recv_data val %d\n ", message->buffer, recv_data);
		if(recv_data == -1){
			warn("%s", client_sockd); 
			message->status_code = 500;
			message->content_length = 0;
			return;
		}
		int write_data = write(fd, message->buffer, recv_data); 
		if(write_data == -1){
			warn("%s", message->filename); 
			message->status_code = 500;
			message->content_length = 0;
			return;
		}
		//warn("%d", fd); 	
		if (log_fd > 0){
			 make_hex(message->buffer, hex_str,recv_data, line_number);
			log_entry_size =  sprintf(log_buffer, "%s", hex_str);//(char*)make_hex(message->buffer, recv_data));
			pwrite(log_fd, log_buffer, log_entry_size, local_offset);
			local_offset = log_entry_size + local_offset;
		}
		//printf("after write: %s\n", message->buffer);
		while(recv_data == BUFFER_SIZE){
			line_number = recv_data + line_number;
			recv_data = recv(client_sockd, message->buffer, BUFFER_SIZE, 0);
			if(recv_data == -1){
				warn("%s", client_sockd); 
				message->status_code = 500;
				message->content_length = 0;
				return;
			}
			write_data = write(fd, message->buffer, recv_data);
			if(write_data == -1){
			warn("%s", message->filename); 
			message->status_code = 500;
			message->content_length = 0;
			return;
			}
			if (log_fd > 0){
				 make_hex(message->buffer, hex_str,recv_data, line_number);
				log_entry_size =  sprintf(log_buffer, "%s", hex_str);//(char*)make_hex(message->buffer, recv_data));
				pwrite(log_fd, log_buffer, log_entry_size, local_offset);
				local_offset = log_entry_size + local_offset;
			}
			//loop to return big data
		}
		if (thread->log_name != NULL){
			log_entry_size =  sprintf(log_buffer, "========\n"); 
			pwrite(log_fd, log_buffer, log_entry_size, local_offset);
			local_offset = log_entry_size + local_offset;
		}
		send(client_sockd, response, strlen(response), 0);
		close(fd);
    }
    //handles a successful head output sends a header and file size also is generalized case for handling errors 
    else{
		snprintf (response, sizeof(response), "%s %d %s\r\nContent-Length: %ld\r\n\r\n", message->httpversion, message->status_code, code_text, message->content_length);
		send(client_sockd, response, strlen(response), 0);
		//printf("%s\n", response);
		
		//case for head
		//case for fail
		if(thread->log_name != NULL){	
			if (strcmp(message->method, "HEAD")==0 && message->status_code==200){
				log_entry_size = sprintf(log_buffer, "%s /%s length %ld\n========\n", message->method, message->filename, message->content_length); //HEAD /abcdefghij0123456789abcdefg length 32\n========\n
				pwrite(log_fd, log_buffer, log_entry_size, local_offset);
				local_offset = log_entry_size + local_offset;
			}
			else{
				printf("log error case\n");
				log_entry_size = sprintf(log_buffer, "FAIL: %s /%s %s --- response %d\n========\n", message->method, message->filename, message->httpversion, message->status_code );//FAIL: GET /abcd HTTP/1.1 --- response 404\n========\n
				pwrite(log_fd, log_buffer, log_entry_size, local_offset);
				local_offset = log_entry_size+ local_offset;
				//pwrite(); fdlogname, response/logbuffer, howmuch to write, offset to start at
				//add count to offset
			}
		}
    }

	close(log_fd);
    free(code_text);
    return;
}

void* request_handler(void* thread){
	struct worker* w_thread = (struct worker*) thread; 
	//recieves the file descriptor
	printf("\033[0;32m[+] In Consumer thread %d...\n\033[0m", w_thread->id);
	//wait while you dont have a job
	//when a job is received start doing shit

	while(true){
		printf("\033[0;32m[+] In Consumer thread %d... doing job\n\033[0m", w_thread->id);
		while(w_thread->client_socket <0){
			printf("waiting\n");
			pthread_cond_wait(&w_thread->condition_variable, w_thread->lock);
			//printf("after wait\n");
		}
		printf("running request process thread %d socket %ld\n", w_thread->id, w_thread->client_socket);
		//sleep(5); for testing to see if multiple threads can run at once
		
		read_http_response(w_thread->client_socket, &w_thread->request);
		process_request(w_thread->client_socket, &w_thread->request, w_thread);
		construct_http_response(w_thread->client_socket, &w_thread->request, w_thread);
		printf("Response Sent\n");
		
		w_thread -> client_socket = -1;
		pthread_cond_signal(&full_var);
		printf("clientsocket:%ld\n", w_thread -> client_socket);
	}
}

void* dispatcher(void* parsed){
	//someone gives me a connection I give to a worker and then keep waiting for connections and handing off new file descriptors
	struct dispatcherInfo* read_info = (struct dispatcherInfo*) parsed;
	printf("\033[0;33m[-] In dispatcher thread...\n\033[0m");
	//handles server connection
	struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
	printf("socket string: %s\n" , read_info-> socket);
    server_addr.sin_port = htons(atoi(read_info->socket)); // port needs to come from data structure //parsed->socket)
    server_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    socklen_t addrlen = sizeof(server_addr);
    /*
        Create server socket
    */
    int server_sockd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_sockd < 0) {
        perror("socket");
    }
    int enable = 1;
    int ret = setsockopt(server_sockd, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(enable));
    ret = bind(server_sockd, (struct sockaddr *) &server_addr, addrlen);
    ret = listen(server_sockd, 5); // 5 should be enough, if not use SOMAXCONN
    if (ret < 0) {
		printf("debug\n");
        return 0;
    }
    struct sockaddr client_addr;
    socklen_t client_addrlen;
	
	struct worker workers[read_info->thread_count];
	//setup worker threads
	pthread_mutex_t lock= PTHREAD_MUTEX_INITIALIZER;
	int is_error = 0;
	for (int i = 0; i < read_info->thread_count-1; i++) {
		workers[i].id = i;
		workers[i].client_socket = -1;
		workers[i].condition_variable = (pthread_cond_t )PTHREAD_COND_INITIALIZER;
		workers[i].lock = &lock;
		workers[i].log_name = (read_info->filename);
		is_error = pthread_create(&workers[i].worker_id, NULL, request_handler, (void *)&workers[i]);
		printf("thread created\n");
		if (is_error != 0){}
	}
	//loop to handle connections
	int count = 0;
	int thread_choice = 0;
	int available = 0;
	//log_ready = 1;
	global_offset = 0;
	int log_fd = -1;
	//int cond  = pthread_cond_signal(&log_var);
	//printf("cond status %d\n", cond);
	if (read_info->filename != NULL){//initialize the log file and create if not there
		log_fd = open(read_info->filename, O_RDWR | O_TRUNC);
		if (log_fd == -1){  
			if (errno == 2){
				log_fd = open(read_info->filename, O_RDWR | O_CREAT | O_TRUNC, 00777);//if file not found // | O_TRUNC
			}
			else {
				warn("%s", read_info->filename); 
				//return;
			}
		}
		close(log_fd);
	}
	while(true){
	//socket stuff
		available = 0;
		printf("server is waiting...\n");
		int client_sockd = accept(server_sockd, &client_addr, &client_addrlen);
		//thread_choice = count % (read_info->thread_count-1);
		//printf("should start %d\n", thread_choice);
		for (int k = 0; k< read_info->thread_count-1; k++){
			//check if worker is avail
			if (workers[k].client_socket == -1){
				thread_choice = k;
				available = 1;
				break;
			}
			//if yes assign and break set flag to found something
		}
		//didnt find it
		while(available == 0){
			printf("waiting for open thread \n");
			pthread_cond_wait( &full_var, &mutex2);
			for (int k = 0; k< read_info->thread_count-1; k++){
			//check if worker is avail
			if (workers[k].client_socket == -1){
				thread_choice = k;
				available = 1;
				break;
			}
		}
		//wake up
		//check who is available

			//if yes assign and break set flag to found something
		}
		//last bit
		
		workers[thread_choice].client_socket = client_sockd;
		pthread_cond_signal(&workers[thread_choice].condition_variable);
		printf("ran process\n");
		count++;
		//assign work to a thread loop to do so
		bool assigned = false;
		/*while(!assigned){
			for (int i = 0; i<read_info->thread_count-1; i++ ){
				if (workers[i].client_socket == -1){
					workers[i].client_socket = client_sockd;
					pthread_cond_signal(&workers[i].condition_variable);
					printf("should start\n");
					assigned = true;
					break;
				}
			}
		}*/
	}

}

/*void* log_sync(void * data){
	//this is where we sync up all of the functionality for writing concurrently
	//code below is for the worker in the bakery algorithm
	int n;
	int choosing[n];
	int number[n];
		
	while (true){
		choosing[i] = 1;
		number[i] = max (number 0 ... number n-1) + 1;
		choosing[i] = 0;
		for (int j = 0; j < n; j++ ){
			//wait while j is choosing 
			while(choosing[j]){}//waits go in brackets
			//wait while j wants to enter and j <<<< i
			while((number[j]!=0) && (number[j] < number[i] || (number[j] == number[j]&& i < j))){}
		}
		//crit section
		number[i];
	}
}*/

int main(int argc, char** argv) {
	int thread_count = 4;
    if (argc < 2) {
	printf("httpserver needs to be called with a port number as its arguement. EX: httpserver 8080\n"); 
	exit(EXIT_FAILURE);
    }
	//handle flags
	char* thread_c_str= NULL;
    char* filename= NULL;
    int index;
    int c;

    opterr = 0;

    while ((c = getopt(argc, argv, "N:l:")) != -1) {
        switch (c) {
            case 'N':
                thread_c_str = optarg;
                break;
            case 'l':
                filename = optarg;
                break;
            case '?':
                if (optopt == 'N' || optopt == 'l') {
                    fprintf(stderr, "Option -%c requires an argument.\n", optopt);
                } else if (isprint (optopt)) {
                    fprintf(stderr, "Unknown option -%c.\n", optopt);
                } else {
                    fprintf(stderr, "Unknown option character \\x%x'.\n", optopt);
                }
            
                return EXIT_FAILURE;
            default:
                abort();
        }
    }
    printf("thread_c_str: %s filename: %s\n", thread_c_str, filename);
	//processing supplied thread count
	if (thread_c_str != NULL){
		thread_count  = atoi(thread_c_str); // add error checking later if necessary
	}
	if (thread_count < 2 ) {
		printf ("cannot have a thread count of less than 2");
		return EXIT_FAILURE;
	}
	
	/*
	Setting up thread infrastructure
	*/
	/*int thread_id[thread_count];
	for (int j= 0; j < thread_count; j++){
		thread_id[j] = j;
	}*/
	pthread_t thread;
	
	/*for(int i = 1; i< thread_count; i++){
		pthread_create(&thread[1], NULL, &worker, &thread_id[i]);
		//printf("worker thread %d", i );
	}*/
	
	
	/*
        Create sockaddr_in with server information
    */
	char port[6];
	//for loop processes rest of input if there is an error will be caught by socket 
    for (index = optind; index < argc; index++) {
        printf("Non-option argument: %s\n", argv[index]);
		strcpy(port, argv[index]);
		//make sure to assign this to port
    }
	//all the information needed to implement dispatcher
	struct dispatcherInfo parsed;
	struct dispatcherInfo* pointer = &parsed;
	(pointer->thread_count) = thread_count;
	
	strcpy(pointer->socket , port);
	printf("%s\n", pointer->socket);
	//strcpy(pointer->filename, filename);
	pointer->filename = filename;
	printf("did it get to create thread\n");
	pthread_create(&thread, NULL, &dispatcher, pointer);
	pthread_join(thread, 0);
	printf("error; %d passed thread\n", 0);
    //struct httpObject message;
	
	
	//testing for helper functions
	//healthcheck
   	//char* log = "log_file";
	//health_check(log);
	//make hex_convert
	
	//hex convert check
	/*char* in = "what oing a asflkdlkjdsalkjsdflkjdsflkjdsflkjsdflkd\n";
	int sdf = 800;
	char out[sdf];
	printf ("%ld \n" , strlen(in));
	make_hex(in , out, strlen(in), 0);
	printf ("%s" , out);*/
	
    return EXIT_SUCCESS;
}


