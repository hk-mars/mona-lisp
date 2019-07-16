
#include "hsearch.h"

#include <string.h>
#include <stdlib.h>


/* [Aho,Sethi,Ullman] Compilers: Principles, Techniques and Tools, 1986
   [Knuth]            The Art of Computer Programming, part 3 (6.4)  */


/* The reentrant version has no static variables to maintain the state.
   Instead the interface of all functions is extended to take an argument
   which describes the current status.  */
typedef struct _ENTRY
{
  unsigned int used;
  ENTRY entry;
} _ENTRY;


/* For the used double hash method the table size has to be a prime. To
   correct the user given table size we need a prime test.  This trivial
   algorithm is adequate because
   a)  the code is (most probably) called a few times per program run and
   b)  the number is small because the table must fit in the core  */
static int
isprime (unsigned int number)
{
  /* no even number will be passed */
  unsigned int div = 3;

  while (div * div < number && number % div != 0)
    div += 2;

  return number % div != 0;
}


/* Before using the hash table we must allocate memory for it.
   Test for an existing table are done. We allocate one element
   more as the found prime number says. This is done for more effective
   indexing as explained in the comment for the hsearch function.
   The contents of the table is zeroed, especially the field used
   becomes zero.  */
int
hcreate_r (long nel, hash_table_s *htab)
{
  /* Test for correct arguments.  */
  if (!htab) return 0;

  /* There is still another table active. Return with error. */
  if (htab->table != NULL) return 0;

  /* Change nel to the first prime number not smaller as nel. */
  nel |= 1;      /* make odd */
  while (!isprime (nel)) nel += 2;

  htab->size = nel;
  htab->filled = 0;

  /* allocate memory and zero out */
  htab->table = (_ENTRY *) calloc (htab->size + 1, sizeof (_ENTRY));
  if (!htab->table) return 0;

  /* everything went alright */
  return 1;
}


/* After using the hash table it has to be destroyed. The used memory can
   be freed and the local static variable can be marked as not used.  */
void
hdestroy_r (hash_table_s *htab)
{
  /* Test for correct arguments.  */
  if (!htab) return;

  /* Free used memory.  */
  free (htab->table);

  /* the sign for an existing table is an value != NULL in htable */
  htab->table = NULL;
}


/* This is the search function. It uses double hashing with open addressing.
   The argument item.key has to be a pointer to an zero terminated, most
   probably strings of chars. The function for generating a number of the
   strings is simple but fast. It can be replaced by a more complex function
   like ajw (see [Aho,Sethi,Ullman]) if the needs are shown.

   We use an trick to speed up the lookup. The table is created by hcreate
   with one more element available. This enables us to use the index zero
   special. This index will never be used because we store the first hash
   index in the field used where zero means not used. Every other value
   means used. The used field can be used as a first fast comparison for
   equality of the stored and the parameter value. This helps to prevent
   unnecessary expensive calls of strcmp.  */
int
hsearch_r 
(
  ENTRY item, ACTION action, ENTRY **retval, hash_table_s *htab
)
{
  unsigned int hval;
  unsigned int count;
  unsigned int len;
  unsigned int idx;
  
  len = (unsigned int)strlen (item.key);
  
  /* Compute an value for the given string. Perhaps use a better method. */
  hval = len;
  count = len;
  while (count-- > 0)
  {
    hval <<= 4;
    hval += item.key[count];
  }

  /* First hash function: simply take the modul but prevent zero. */
  idx = hval % htab->size + 1;

  if (htab->table[idx].used)
  {
    
    /* Further action might be required according to the action value. */
    if (htab->table[idx].used == hval && 
      strcmp (item.key, htab->table[idx].entry.key) == 0)
    {
      *retval = &htab->table[idx].entry;
      return 1;
    }
  
    /* Second hash function, as suggested in [Knuth]. */
    unsigned int hval2 = 1 + hval % (htab->size - 2);
    unsigned int first_idx = idx;
  
    do
    {
      /* Because SIZE is prime this guarantees to step through all available indeces. */
      if (idx <= hval2) {
        idx = htab->size + idx - hval2;
      }
      else {
        idx -= hval2;
      }
      
      /* If we visited all entries leave the loop unsuccessfully. */
      if (idx == first_idx) break;
      
      /* If entry is found use it. */
      if (htab->table[idx].used == hval && 
        strcmp (item.key, htab->table[idx].entry.key) == 0)
      {
        *retval = &htab->table[idx].entry;
        return 1;
      }
    }while (htab->table[idx].used);
    
  }

  /* An empty bucket has been found. */
  if (action == ENTER)
  {
    /* If table is full and another entry should be entered then return with error. */
    if (htab->filled == htab->size)
    {
      *retval = NULL;
      return 0;
    }
    
    htab->table[idx].used  = hval;
    htab->table[idx].entry = item;
    
    ++htab->filled;
    
    *retval = &htab->table[idx].entry;
    return 1;
  }
  
  *retval = NULL;
  return 0;
}


int 
hcreate(hash_table_s *htab, long nel)
{
  int ret;
  
  if (!htab) return 0;
  
  ret = hcreate_r(nel, htab);
  
  return ret;
}


ENTRY *
hsearch(hash_table_s *htab, ENTRY item, ACTION action)
{
  ENTRY *result;
  
  if (!htab) return NULL;
  
  (void) hsearch_r(item, action, &result, htab);
  
  return result;
}


void 
hdestroy (hash_table_s *htab)
{
  if (!htab) return;
  
  hdestroy_r(htab);
}


