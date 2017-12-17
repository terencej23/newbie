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

//  // testing the suite
// int main(){
// 	// LinkedList of numbers 
// 	struct NumNode mynode;
// 	struct NumNode* head = &mynode;

// 	// LinkedList of strings
// 	struct StringNode stringNode;
// 	struct StringNode* str_head = &stringNode;

// 	// prints first 100 numbers in a LinkedList
// 	for (int i = 0; i < 100; ++i)
// 	{
// 		head->val = i;
// 		head->next = malloc(sizeof(struct NumNode*));
// 		head = head->next;
// 	}
// 	head->next = NULL;
// 	head = &mynode;

// 	// prints strings with a new char* appended
// 	for (int i = 0; i < 100; ++i)
// 	{
// 		char* new_string = "hello";
// 		str_head->val = new_string;
// 		str_head->next = malloc(sizeof(struct StringNode*));
// 		str_head = str_head->next;
// 	}
// 	str_head->next = NULL;
// 	str_head = &stringNode;


// 	print_list_num(head);
// 	print_list_string(str_head);
// }