#include <stdio.h>
#include <stdlib.h>
struct NumNode {
	int val;
	struct NumNode* next;
};

struct StringNode {

	char* val;
	struct StringNode* next;
};


void print_list_num(struct NumNode* head){
	// prints the list of items in LinkedList
	while(head){
		printf("%d\n", head->val);
		head = head->next;
	}
}

void print_list_string(struct StringNode* head){
	// prints the list of items in LinkedList
	while(head){
		printf("%s\n", head->val);
		head = head->next;
	}
}