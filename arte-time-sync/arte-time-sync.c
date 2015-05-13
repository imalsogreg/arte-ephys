#include <sys/socket.h>
#include <stdio.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <time.h>
#include <stdint.h>

struct time_record {
	int_fast64_t seconds;
	int_fast64_t nanoseconds;
};

int main(int argc, char ** argv) {
	int sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock == -1) {
		perror("creating datagram socket");
		return 1;
	}
	int y = 1;
	if (setsockopt(sock, SOL_SOCKET, SO_BROADCAST, &y, sizeof y) == -1) {
		perror("enabling broadcast");
		return 2;
	}
	struct sockaddr_in addr = { 0 };
	addr.sin_family = AF_INET;
	/* TODO: take as param */
	addr.sin_port = htons(6080);
	struct in_addr ip = { 0 };
	/* TODO: take as param */
	ip.s_addr = inet_addr("127.0.0.1");
	addr.sin_addr = ip;
	/* TODO: take as param */
	struct timespec sleep_interval = { 0 };
	sleep_interval.tv_sec = 1;
	do {
		struct timespec time;
		if (clock_gettime(CLOCK_MONOTONIC, &time) == -1) {
			perror("getting monotonic time");
			continue;
		}
		struct time_record rec = {
			(int_fast64_t) time.tv_sec,
			(int_fast64_t) time.tv_nsec
		};
		char buf[16]; // two 64-bit integers
		for (char i = 0; i < 8; ++ i) {
			buf[i] = (rec.seconds >> i * 8) & 0xFF;
			buf[i + 8] = (rec.nanoseconds >> i * 8) & 0xFF;
		}
		ssize_t res = sendto(
			sock,
			buf,
			sizeof buf,
			0,
			(struct sockaddr *) &addr,
			sizeof addr
		);
		if (res != sizeof addr) {
			if (res == -1)
				perror("sending time record");
			else
				fprintf(
					stderr,
					"incomplete sendto sending time record"
				);
		}
	} while (nanosleep(&sleep_interval, NULL) == 0
			|| (perror("sleeping"), 1));
}
