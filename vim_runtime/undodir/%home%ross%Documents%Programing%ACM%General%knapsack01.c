Vim�UnDo� �y��UZ�&����z^7q��~��'�����   .                                   O�X�    _�                     
        ����                                                                                                                                                                                                                                                                                                                                                             O�Xx     �   	   
           5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             O�Xz     �                 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             O�X|     �                 5�_�                           ����                                                                                                                                                                                                                                                                                                                                                             O�X�     �                ,	scanf("%d %d", &items[i][0], &items[i][1]);5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             O�X�     �   4   6              return 0;�   3   5          3    printf("Max benefit:%d\n", b[wtot][num_items]);�   1   3              }�   0   2          	    */	   	�   /   1          	   printf("\n");�   .   0          	   }�   -   /          	   printf("\n");�   ,   .          	   printf("%d ", b[jj][ii]);�   +   -           	   for (int jj=0;jj<=wtot;jj++)�   *   ,          %	   for(int ii=0;ii<=num_items;ii++){�   )   +          	    /*	�   (   *          !	// Print out if you want to see	�   %   '          	}�   #   %          B		b[j][i] = max(b[j][i-1], b[j - items[i][0]][i-1] + items[i][1]);�   "   $          
	    else �       "          		b[j][i] = b [j][i-1];�      !          	    if (items[i][0] > j)�                	for (int j=1;j<= wtot; j++){�                    {�                $    for (int i =1;i<= num_items;i++)�                	    // DP�                	b[i][0]=0;�                    for(int i=0;i<=wtot;i++)�                	b[0][i]=0;�                #    for (int i=0;i<= num_items;i++)�                8    int b[wtot+1][num_items+1]; // benefit array b[w, k]�                    }�                1      scanf("%d %d", &items[i][0], &items[i][1]);�                %    for(int i =1;i<= num_items; i++){�   
              �   	             4    int items[num_items+1][2]; // Off set items by 1�      
          &    scanf("%d %d", &wtot, &num_items);�                    int wtot;�                    int num_items;5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             O�X�     �                 5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             O�X�     �                 5�_�      	                      ����                                                                                                                                                                                                                                                                                                                                                             O�X�     �                 5�_�      
           	           ����                                                                                                                                                                                                                                                                                                                                                             O�X�     �                 5�_�   	              
   !        ����                                                                                                                                                                                                                                                                                                                                                             O�X�     �       !           5�_�   
                 "        ����                                                                                                                                                                                                                                                                                                                                                             O�X�     �   !   "           5�_�                    -        ����                                                                                                                                                                                                                                                                                                                                                             O�X�     �   ,   -           5�_�                     "        ����                                                                                                                                                                                                                                                                                                                                                             O�X�    �   !   "           5��