#include<err.h>
#include<arpa/inet.h>
#include<netdb.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<sys/socket.h>
#include<sys/types.h>
#include<unistd.h>
#include<errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdbool.h> 
#include <time.h>
//global vars some of which need synchronization
int request_count = 0; // this will need synchronization
	pthread_mutex_t lock= PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex1= PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t global_var = PTHREAD_COND_INITIALIZER;
pthread_cond_t dispatcher_var = PTHREAD_COND_INITIALIZER;
struct serverInstance{
	int running;
	//add stuff as it is needed to make this thing work
	int server_fd; //fd for where this server is this might be bad just calc on site
	int errors;
	int entries;
	int port;
	int client_fd; //fd for file from the client connecting to lb dont know if this is used
};
struct serverList{
	struct serverInstance* servers;
	int size;
	int requests;
	int request_limit;
};
struct worker_info{
	struct serverInstance* servers;
	int size; 
	int client_fd;
	int id;
	int request_limit;
	pthread_t worker_id;
	pthread_cond_t condition_var;
	pthread_mutex_t* lock;
	//condvar //lock pointer
	//additional 
};
struct queue{
	int fd; 
	struct queue* next; 
};
//run health check
struct dispatcher_info{
	int * port;
	int port_count;
	int requests;
	int parallel_cons;
};
/*
 * client_connect takes a port number and establishes a connection as a client.
 * connectport: port number of server to connect to
 * returns: valid socket if successful, -1 otherwise
 */
int client_connect(uint16_t connectport) {
    int connfd;
    struct sockaddr_in servaddr;

    connfd=socket(AF_INET,SOCK_STREAM,0);
    if (connfd < 0)
        return -1;
    memset(&servaddr, 0, sizeof servaddr);

    servaddr.sin_family=AF_INET;
    servaddr.sin_port=htons(connectport);

    /* For this assignment the IP address can be fixed */
    inet_pton(AF_INET,"127.0.0.1",&(servaddr.sin_addr));

    if(connect(connfd,(struct sockaddr *)&servaddr,sizeof(servaddr)) < 0)
        return -1;
    return connfd;
}

/*
 * server_listen takes a port number and creates a socket to listen on 
 * that port.
 * port: the port number to receive connections
 * returns: valid socket if successful, -1 otherwise
 */
int server_listen(int port) {
    int listenfd;
    int enable = 1;
    struct sockaddr_in servaddr;

    listenfd = socket(AF_INET, SOCK_STREAM, 0);
    if (listenfd < 0)
        return -1;
    memset(&servaddr, 0, sizeof servaddr);
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = htons(INADDR_ANY);
    servaddr.sin_port = htons(port);

    if(setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(enable)) < 0)
        return -1;
    if (bind(listenfd, (struct sockaddr*) &servaddr, sizeof servaddr) < 0)
        return -1;
    if (listen(listenfd, 500) < 0)
        return -1;
    return listenfd;
}

/*
 * bridge_connections send up to 100 bytes from fromfd to tofd
 * fromfd, tofd: valid sockets
 * returns: number of bytes sent, 0 if connection closed, -1 on error
 */
int bridge_connections(int fromfd, int tofd) {
    char recvline[4000];
    int n = recv(fromfd, recvline, 4000, 0);
    if (n < 0) {
        printf("connection error receiving\n");
        return -1;
    } else if (n == 0) {
        printf("receiving connection ended\n");
        return 0;
    }
    recvline[n] = '\0';
    //printf("%s", recvline);
    //sleep(1);
    n = send(tofd, recvline, n, 0);
    if (n < 0) {
        printf("connection error sending\n");
        return -1;
    } else if (n == 0) {
        printf("sending connection ended\n");
        return 0;
    }
    return n;
}

/*
 * bridge_loop forwards all messages between both sockets until the connection
 * is interrupted. It also prints a message if both channels are idle.
 * sockfd1, sockfd2: valid sockets
 */
void bridge_loop(int sockfd1, int sockfd2) {
    fd_set set;
    struct timeval timeout;

    int fromfd, tofd;
    while(1) {
        // set for select usage must be initialized before each select call
        // set manages which file descriptors are being watched
        FD_ZERO (&set);
        FD_SET (sockfd1, &set);
        FD_SET (sockfd2, &set);

        // same for timeout
        // max time waiting, 5 seconds, 0 microseconds
        timeout.tv_sec = 2;
        timeout.tv_usec = 0;

        // select return the number of file descriptors ready for reading in set
        switch (select(FD_SETSIZE, &set, NULL, NULL, &timeout)) {
            case -1:
                printf("error during select, exiting\n");
                return;
            case 0:
                printf("both channels are idle, waiting again\n");
				return; // this might need a revision
                continue;
            default:
                if (FD_ISSET(sockfd1, &set)) {
                    fromfd = sockfd1;
                    tofd = sockfd2;
                } else if (FD_ISSET(sockfd2, &set)) {
                    fromfd = sockfd2;
                    tofd = sockfd1;
                } else {
                    printf("this should be unreachable\n");
                    return;
                }
        }
        if (bridge_connections(fromfd, tofd) <= 0)
            return;
    }
}

void* servers_health (void* healthcheck_info){
	struct serverList* server_list = (struct serverList*) healthcheck_info;
		//we determine the health of the server here and it is used everwhere else 
	//when requests = 5 or when time is over said limit
	char* healthcheck_text = "GET /healthcheck HTTP/1.1\r\n\r\n";//length of str 
	char buffer[400];
	int has_run = 0;
	//server->server_fd = connfd; 
	while(true){
		struct timespec t; // time stuff might need a second look
		int result = clock_gettime(CLOCK_REALTIME, &t);
		//printf("result %d\n" , has_run);
		if (result == -1) {
			perror("clock_gettime");
			//exit(EXIT_FAILURE);
		}
		t.tv_sec += 3;
		int clear = 1;
		//server_list->requests 
		
		/*while(request_count % server_list ->request_limit == 0 || has_run == 0){//wait while conditions to do health check not met 
			//condition here not quite right yet
			//printf("waiting\n");
			clear = pthread_cond_timedwait(&global_var, &lock, &t);  //const struct timespec *restrict abstime
			//printf("clear value; %d \n", clear);
			if ( clear == ETIMEDOUT){
				
				has_run = 0;
				//printf("hit timeout%d\n" , has_run);
				break;
			}
		}*/
		//
		if (request_count % server_list ->request_limit == 0 && has_run == 0){
			printf("first run\n");
		}
		else{
		clear = pthread_cond_timedwait(&global_var, &lock, &t); 
		}
		//printf("after timed wait\n");
		//running health check 
		//pthread_mutex_lock(&);
		for(int j = 1; j < server_list->size; j++){//if this receives no response set entries to -1
			memset(buffer,0,sizeof(buffer));
			struct serverInstance* server= &server_list->servers[j];
			int connfd = client_connect(server->port);
			if (connfd == -1){
				server->errors = -1;
				server->entries = -1;
				printf("errors and entries %d %d\n", server->errors, server->entries); 
				has_run = 1;
				continue;
			}
			int write_count = send(connfd, healthcheck_text, 29,0); // send get request and length of request
			//receive from request
			//printf("server fd %d\n", connfd);
			clock_t start  = clock();
			int start_time = start/CLOCKS_PER_SEC;
			int timeout = 0;
			int code = 0;
			int offset = recv(connfd, buffer, 400, 0 );
			while(recv(connfd, &buffer[offset-1], 400, 0 ) != 0 ){
				//
				//read in to the buffer while there is input
				if ((clock()/CLOCKS_PER_SEC)- start_time>3) {
					timeout = 1;
					break;
				}
				//verify the buffer isnt getting too fucked up
			}
			if(timeout != 1){
				//doesnt timeout
				//printf("buffer contents: %s\n", buffer);
				//printf("buffer contents: %s\n", buffer);
				sscanf(buffer, "HTTP/1.1 %d ",  &code );
				if(code == 200){
					int format = sscanf(buffer, "HTTP/1.1 200 OK Content-Length: %d %d %d", &code , &server->errors, &server->entries);
						//printf("format: %d\n", format);
					if (format != 3){
						server->errors = -1;
						server->entries = -1;
					}
				}
				else {
					server->errors = -1;
					server->entries = -1;
				}
			}
			else{
				server->errors = -1;
				server->entries = -1;
			}
			//pthread_mutex_unlock(&mutex1);
			//printf("buffer contents2: %s\n", buffer);
			//strcpy(buffer, "");
			printf("errors and entries %d %d\n", server->errors, server->entries);
			has_run = 1;			
			//connfd = client_connect(server->port);
			//server->server_fd = connfd; // this makes it work but there might be a race condition for this port # now
			//strcpy(buffer, "");
			
			//need to modify shared request counter and possibly shared entry thing for servers
			//parse the input
			//then should be done
		
		}
		
	}
}

void* parallel(void* server_info){
	struct worker_info* server_list = (struct worker_info*) server_info;
	while (true){
		while(server_list->client_fd < 0){
			printf("waiting worker\n");
			pthread_cond_wait(&server_list->condition_var, server_list->lock);
		}
		printf("after waiting worker\n");
		//request_count++; //watch this var it needs sync later
		int best_server = 1000000000 ;
		int best_index = 0;
		char* avail_error = "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n";
		for (int j = 1 ; j< server_list->size; j++){
			struct serverInstance* server= &server_list->servers[j];
			//check to see who is biggest
			if((server->entries) <= best_server && server->entries > -1){
				if((server->entries) == best_server){
					if (server->errors <= server_list->servers[best_index].errors&& server->errors > -1){
						best_server = server->entries;
						best_index = j;
					}
				}
				//else{
				best_server = server->entries;
				//printf ("index: %d entries %d \n", j , server->entries);
				best_index = j;
				//}
			}
		}
		//test stuff to work on making sure responses are correct
		//best_index = 1;
		if (best_index == 0){
			//no servers are currently available 
			//return 500 error 
			int write_data = send(server_list->client_fd, avail_error, 57,0);
			printf("didnt send request to a server\n");
		}
		else{
							// This is a sample on how to bridge connections.
		// Modify as needed.
			int serverfd = client_connect(server_list->servers[best_index].port); //new conn for the server
			bridge_loop(server_list->client_fd, serverfd);
		}
		//we check the health of all the servers then forward 
		//check the values for all the servers and see what is best 
		//reset fd so thread is ready for a new thing
		//end of statement synch stuff
		//printf("before lock\n");
		pthread_mutex_lock(&mutex1);
		request_count++;
		//printf("requestcount in crit sec : %d \n", request_count);
		
		if (request_count % (server_list->request_limit) ==0){//this needs to not be hard coded later 
			pthread_cond_signal(&global_var);
			//printf("signal healthcheck\n");
		}
		printf("requestcount : %d \n", request_count);
		pthread_mutex_unlock(&mutex1);
		server_list -> client_fd = -1; 
		pthread_cond_signal(&dispatcher_var);
	}
}

void* dispatcher(void* dis_info){
	struct dispatcher_info* parsed = (struct dispatcher_info*) dis_info;
	    int connfd, listenfd, acceptfd;
    uint16_t connectport, listenport;
	
//lock
		printf ("port count %d\n", parsed->port_count);
	struct serverInstance servers[parsed->port_count];
	for(int j = 1; j < parsed->port_count; j++){
		connectport = (parsed->port[j]);
		if ((connfd = client_connect(connectport)) < 0)
			//err(1, "failed connecting");
		//printf("did connect fd: %d\n", connfd);
		//connected to a server and create a server info object
		servers[j]. running = 0;
		servers[j].errors = -1;
		servers[j].entries = -1;
		servers[j].port = connectport;
		servers[j]. server_fd = connfd;
	}
	//int request_count = 0;
	listenport = (parsed->port[0]); // establish place to listen from
	if ((listenfd = server_listen(listenport)) < 0)
        err(1, "failed listening");
	//request q setup
	struct queue* conn_list;
	struct queue first_entry;
	first_entry.fd = -40;
	first_entry.next = NULL;
	conn_list = &first_entry;
	
	//set up heathcheck thread
	struct serverList health;
	health.servers = servers;
	health.size = parsed->port_count;
	health.requests = request_count % parsed->requests; // requests is expecting a requests % r
	health.request_limit = parsed->requests;
	pthread_t thread;
	pthread_create(&thread, NULL, &servers_health, &health);
	
	//setup worker threads 
	struct worker_info workers[parsed->parallel_cons];
	for (int i = 0; i < parsed->parallel_cons; i++){
		workers[i].servers = servers;
		workers[i].size = parsed->port_count;
		workers[i].client_fd = -1;
		workers[i].condition_var = (pthread_cond_t )PTHREAD_COND_INITIALIZER;
		workers[i].id = i;
		workers[i].lock = &lock;
		workers[i].request_limit = parsed->requests;
		int is_error = pthread_create(&workers[i].worker_id, NULL, parallel, (void *)&workers[i]);
		printf("thread created\n");
		if (is_error != 0){}
	}
	//signal healthcheck run once
	//pthread_mutex_lock(&mutex1);
	pthread_cond_signal(&global_var);
	//pthread_mutex_unlock(&mutex1);
	while(true){
		printf("server is listening\n");
		//struct queue* iter  = conn_list;
		/*while (iter->fd > -1){
			printf("entry: %d\n", conn_list->fd);
			iter = iter->next;
		}*/
		//printf("entry: %d\n", conn_list->fd);
		while (conn_list->fd != -40){
			
		}
		//printf("queue head %d\n" 
		if ((acceptfd = accept(listenfd, NULL, NULL)) < 0)
			err(1, "failed accepting");
		else {
			struct queue new_entry;
			new_entry.fd = acceptfd;
			new_entry.next = conn_list;
			conn_list = &new_entry;
		}
		while (conn_list->fd > -1){
			//printf("clear queue2\n");
			//get rid of entries that are on the queue then loop back keep going 
			//conn_list->fd; //take this value and send it away to a thread 
			//printf("popping %d\n", conn_list->fd ); 
			int avail_worker = -1;
			for (int i = 0; i < parsed->parallel_cons; i++){
				//cycle through servers and find out which ones are currently 
				if (workers[i].client_fd == -1){
					avail_worker = i; // picks the thread to send the job to
					
					break;
				}
			}
			while (avail_worker == -1){
				printf("everyone busy\n");
				pthread_cond_wait(&dispatcher_var, &lock);//break; //avoid doing anything as everyone is busy right now
			for (int i = 0; i < parsed->parallel_cons; i++){
				//cycle through servers and find out which ones are currently 
				if (workers[i].client_fd == -1){
					avail_worker = i; // picks the thread to send the job to
					
					break;
				}
			}
			}
			printf("assigned to %d\n", avail_worker ); 
			workers[avail_worker].client_fd = conn_list->fd;
			pthread_cond_signal(&workers[avail_worker].condition_var);
			struct queue* temp  = conn_list->next; //entry we worked with
			
			//free(conn_list); // free entry we worked on
			conn_list =temp; //set connlist to new head
		}
		//should be listening forever now and accepting as it goes
		//store connection in a queue
		//set a flag indicating a request is ready to be processed 
		//a thread picks up the request 
		//the request is removed from the queue
		//keep doing this until the end of the list is reached then keep looking for new cons

		//decide which one
		//assign connections/ requests to a server
	}
}
//void* timeout (){}
int main(int argc,char **argv) {

	int c;
	int parallel_cons = 4;
	int requests = 5;
	//int time_out = 5; // this value changes as we go 

    if (argc < 3) {
        printf("missing arguments: usage %s port_to_connect port_to_listen", argv[0]);
        return 1;
    }

    // Remember to validate return values
    // You can fail tests for not validating
    /*connectport = atoi(argv[1]);
    listenport = atoi(argv[2]);
    if ((connfd = client_connect(connectport)) < 0)
        err(1, "failed connecting");
    if ((listenfd = server_listen(listenport)) < 0)
        err(1, "failed listening");
    if ((acceptfd = accept(listenfd, NULL, NULL)) < 0)
        err(1, "failed accepting");*/
	char* requests_str = NULL;
	char* parallel_cons_str = NULL;
	//parse for flags
	 while ((c = getopt(argc, argv, "N:R:")) != -1) {
        switch (c) {
            case 'R':
                requests_str= optarg;
                break;
            case 'N':
                parallel_cons_str= optarg;
                break;
            case '?':
                if (optopt == 'N' || optopt == 'R') {
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
	if (requests_str != NULL){
		requests = atoi(requests_str);
	}
	if (parallel_cons_str != NULL){
		parallel_cons= atoi(parallel_cons_str);
	}
		printf("requests: %d parallel_cons %d \n", requests, parallel_cons);
	char port[20][6]; // array of strings 
	int port_count = 0; // assigning stuff to an array 
	 for (int index = optind; index < argc; index++) {
        printf("Non-option argument: %s\n", argv[index]);
		strcpy(port[port_count], argv[index]);
		port_count++;
		//make sure to assign this to port
    }
	if (port[0] == NULL){
		//something wrong nothing to host loadbalancer
		printf("needs a socket to host load balancer\n");
		return EXIT_FAILURE;
	}
	if (port[1] == NULL){
		//check if at least one port to host an httpserver
		printf("needs at least one httpserver\n");
		return EXIT_FAILURE;
	}
	//we have count of ports so port_count = servers managed by this 
	//select could be very helpful
	int port_number[20];
	for(int i = 0; i < port_count; i++){
		port_number[i] = atoi(port[i]);
	}
	struct dispatcher_info starter_info;
	starter_info.parallel_cons = parallel_cons;
	starter_info.requests = requests;
	starter_info.port_count = port_count;
	starter_info.port = port_number;
		pthread_t thread;
		 pthread_create(&thread, NULL, dispatcher, (void *)&starter_info);
		printf("thread created\n");
	pthread_join(thread, 0);
	
	//establish connection to all of the servers

}