
import math
#top level def for start and stop and possibly make it an object
#files are opened from here
#file names
#1b_benchmark.dev.tokens
#1b_benchmark.train.tokens
#1b_benchmark.test.tokens
#python dictionary for storage

#start stop and unk
UNK = "UNK"
START = "[START]"
STOP = "[STOP]"

#parse in sentences store in list of strings (sentences)
def parser(f):
    sentence_list = []
    file = open(f, encoding="utf8")
    count = 0
    for line in file:
       #print (line)
       line = line.rstrip()
       sentence_list.append(START + " "+ line +" "+ STOP)
       count +=1
    file.close()
    #print (count)
    return sentence_list

#turns a list of strings in to a list of token lists
def token_sentences(dic, stcs): # something is fishy here
    #no starts in this thing
    words = 0
    #turns stcs into a list of sentences split into tokens so useable for perplexity analysis
    separated = []
    for count in range (0, len(stcs)):
        separated.append(stcs[count].split(" "))
        separated[count].remove(START)
        #print (separated[count])

        separated[count]= replace(dic, separated[count])  # this replaces uncommon words with unk
        words += len(separated[count])
    #this might need to replace some words with unk has dic so if lookup is unable to find word then replace with UNK
    #print ("token count" )
    #print (words)
    return separated

#replaces uncommon tokens in a list of token lists
def replace(dic, sentence):  #might be supposed to do some removing unclear possibly stupid on piazza
    for i in range(0,len(sentence)):
        if (sentence[i]) in dic or sentence[i] == START:
            continue
        else:
            sentence[i] = UNK
    return sentence

#this creates a dictionary with words from training data
#dictionary is (token: count)
def training(stcs):
    dic = {}
    #creates tokens and gets counts
    #seperate line
    #remove start
    #split everything on spaces resulting in a list for the sentences
    #remove start token
    #start adding to dictionary
    for count in range (0, len(stcs)):
        separated = stcs[count].split(" ")
        separated.remove(START)
        #update to change count remove to do unk thing
        #sets up unk with value zero

        for i in range(0, len(separated)):
            if dic.get(separated[i])== None:
                dic.update({separated[i]: 1})
            else:
                incremented = dic.get(separated[i]) + 1
                dic.update({separated[i]: incremented})
    return dic

#this handles replacing uncommon tokens with UNK in dictionary and keeps unk counts
def unknown(dic):
    #does unknowns in the dic but not in the data so need something else for that
    key = list (dic)

    dic.update({UNK: 0})
    for i in range (0,len(key)):
        word_count = 0
        if (dic.get(key[i])) < 3:
            word_count = dic.get(key[i])
            del dic[key[i]]
            incremented = dic.get(UNK) + word_count
            dic.update({UNK: incremented})
    return dic

#gets word count for a dictionary
def word_count(dic):
    #gets total number of words or can just sum a dictionary's values
    count = 0
    count = sum (dic.values())
    return count

#this function calculates perplexity for unigram model takes training data and training probs
def unigram_perplexity(dic, dic_prob, sentences):
    #map to second dic?
    #this does probablility of all queries and does perplexity now
    m = 0
    for count in range (0, len(sentences)):
        m +=len(sentences[count])
     #this should be word count from training set
    #think this needs a second input of the sentences
    prob_data = 0
    for i in range(0,len(sentences)):
        #print (i)
        prob_sntc = 0
        for k in range(0, len(sentences[i])):

            #if dic.get(sentences[i][k], UNK) == UNK:
            prob_sntc += math.log(dic_prob.get(sentences[i][k]), 2)
            #else:
               # prob_sntc = prob_sntc * (dic.get(sentences[i][k]) / m)
           # print (prob_sntc)
        prob_data = prob_data +  prob_sntc
        #print (prob_data)
        #calc probability
    prob_data = 2 **((-1) * prob_data / m)
    return prob_data

#returns a dictionary associating tokens with probablility in training data
def unigram_tokens(dic):
    #this thing does probabiliies for each word and stores in a second dictionary
    probability={}
    m = word_count(dic)
    prb_list = list(dic)
    for i in range(0, len(dic)):
        probability.update({prb_list[i]: (dic.get(prb_list[i]) / m)})
    return probability

#turns a list of strings into list of bigram lists
def bigram_sentence(dic, stcs):
    #will make list of sentences into a list of bigram lists made from sentences
    #start should not be removed in this case
    separated = []
    list_sentences = []

    for count in range(0, len(stcs)):
        pair_list = []
        separated.append(stcs[count].split(" "))
        #separated[count].remove(START) we want start in the bigram data
        # print (separated[count])
        # should preserve start in this case
        separated[count] = replace(dic, separated[count])  # this replaces uncommon words with unk
        for i in range(0, len(separated[count])-1):
            pair_list.append((separated[count][i], separated[count][i+1]))  # maybe make the pair a list with a list
        #print (pair_list)
        list_sentences.append(pair_list)
    return list_sentences

#makes a dictionary with bigrams and the number of times they appear
def bigram_probability(dic, bigram_list):
    bigram_prob = {}
    for count in range(0, len(bigram_list)):
        #print (bigram_list[count])
        for i in range(0, len(bigram_list[count])):
            if bigram_prob.get(bigram_list[count][i])== None :
                bigram_prob.update({bigram_list[count][i]: 1})
                #print ("creating new entry)")
            else:
                incremented = bigram_prob.get(bigram_list[count][i]) + 1
                bigram_prob.update({bigram_list[count][i]: incremented})
                #print (incremented)
    return bigram_prob

#calculates perplexity for bigram model
def bigram_perplexity(dic, bigram_prob, sentences):
    #probability of a word given the word before it
    #so prob of the duo with the rest of the duos as a pool
    # then do the perplexity thing on the
    # prob of first word, second word / first word
    #currently takes number of pairs + 1 for token count for end
    #then does the same thing as unigram but for prob it does p(first,second)/p(first)
    #should be theoretically ok
    m = 0
    for count in range(0, len(sentences)):
        m += len(sentences[count])
    # this should be word count from training set
    # think this needs a second input of the sentences
    prob_data = 0
    for i in range(0, len(sentences)):
        # print (i)
        prob_sntc = 0
        for k in range(0, len(sentences[i])):
            if bigram_prob.get(sentences[i][k]) == None:
                return "perplexity is infinite"
            if sentences[i][k][0]==START:
                prob_sntc += math.log(bigram_prob.get(sentences[i][k]) / dic.get(STOP), 2) # this needs to do actual start count instead of an error
            else:
                prob_sntc += math.log(bigram_prob.get(sentences[i][k])/dic.get(sentences[i][k][0]), 2) #this is currently trying to do log of 0 in some cases which is uhoh
            # else:
            # prob_sntc = prob_sntc * (dic.get(sentences[i][k]) / m)
        # print (prob_sntc)
        prob_data = prob_data + prob_sntc
        # print (prob_data)
        # calc probability
    prob_data = 2 ** ((-1) * prob_data / m)
    return prob_data

#turns a list of strings into list of trigram lists
#first entry of each trigram list is a bigram (start, [TOKEN])
def trigram_sentence(dic, stcs):
    #will make list of sentences into a list of bigram lists made from sentences
    #start should not be removed in this case
    separated = []
    list_sentences = []

    for count in range(0, len(stcs)):
        pair_list = []
        separated.append(stcs[count].split(" "))
        #separated[count].remove(START) we want start in the bigram data
        # print (separated[count])
        # should preserve start in this case
        separated[count] = replace(dic, separated[count])  # this replaces uncommon words with unk
        pair_list.append ((separated[count][0], separated[count][1])) # this adds a bigram out front
        for i in range(0, len(separated[count])-2):
            pair_list.append((separated[count][i], separated[count][i+1], separated[count][i+2]))  # maybe make the pair a list with a list
        #print (pair_list)
        list_sentences.append(pair_list)
    return list_sentences

#makes a dictionary with trigrams and the number of times they appear
def trigram_probability(dic, trigram_list):
    trigram_prob = {}
    for count in range(0, len(trigram_list)):
        #print (bigram_list[count])
        if trigram_prob.get(trigram_list[count][0]) == None: # handles random bigram outfront this was just added
            trigram_prob.update({trigram_list[count][0]: 1})
            # print ("creating new entry)")
        else:
            incremented = trigram_prob.get(trigram_list[count][0]) + 1
            trigram_prob.update({trigram_list[count][0]: incremented})

        for i in range(1, len(trigram_list[count])):
            if trigram_prob.get(trigram_list[count][i])== None :
                trigram_prob.update({trigram_list[count][i]: 1})
                #print ("creating new entry)")
            else:
                incremented = trigram_prob.get(trigram_list[count][i]) + 1
                trigram_prob.update({trigram_list[count][i]: incremented})
                #print (incremented)
    return trigram_prob

#calculates perplexity for trigram model
def trigram_perplexity(bigram_dic, trigram_prob, sentences):
    #probability of a word given the word before it
    #so prob of the duo with the rest of the duos as a pool
    # then do the perplexity thing on the
    # prob of first word, second word / first word
    #currently takes number of pairs + 1 for token count for end
    #then does the same thing as unigram but for prob it does p(first,second)/p(first)
    #should be theoretically ok
    m = 0
    for count in range(0, len(sentences)):
        m += len(sentences[count])
    # this should be word count from training set
    # think this needs a second input of the sentences
    prob_data = 0
    for i in range(0, len(sentences)):
        # print (i)
        prob_sntc = 0
        #handle bigram out front
        if bigram_dic.get(sentences[i][0]) == None:
            return "perplexity is infinite"
        else:
            prob_sntc += math.log(bigram_dic.get((sentences[i][0])) / 61530, 2)

        for k in range(1, len(sentences[i])):
            if trigram_prob.get(sentences[i][k]) == None:
                return "perplexity is infinite"
            if bigram_dic.get((sentences[i][k][0], sentences[i][k][1]))== None:
                #prob_sntc += math.log(trigram_prob.get(sentences[i][k]) / dic.get(STOP), 2) # this needs to do actual start count instead of an error\
                return "zero denominator"
            else:
                prob_sntc += math.log(trigram_prob.get(sentences[i][k])/bigram_dic.get((sentences[i][k][0], sentences[i][k][1])), 2) #this is currently trying to do log of 0 in some cases which is uhoh
            # else:
            # prob_sntc = prob_sntc * (dic.get(sentences[i][k]) / m)
        # print (prob_sntc)
        prob_data = prob_data + prob_sntc
        # print (prob_data)
        # calc probability
    prob_data = 2 ** ((-1) * prob_data / m)
    return prob_data

#calculates perplexity based on all 3 models with lam1-3 as weights respectively
def smoothing(lam1, lam2, lam3, dic, unigram_prob, bigram_prob, trigram_prob, sentences):
    # call with different lam values

    # this should be word count from training set
    # think this needs a second input of the sentences
    uni_sentences = token_sentences(dic, sentences)
    bi_sentences = bigram_sentence (dic, sentences)
    tri_sentences = trigram_sentence(dic, sentences)

    m = 0
    for count in range(0, len(uni_sentences)):
        m += len(uni_sentences[count])

    prob_data = 0

    for i in range(0, len(tri_sentences)):
        # print (i)
        prob_sntc = 0
        #run through once for bigram

        # unigram prob
        ''' if bigram_dic.get(sentences[i][0]) == None:
            return "perplexity is infinite"
        else:
            prob_sntc += math.log(bigram_.get((sentences[i][0])) / 61530, 2)
        '''
        for k in range(0, len(tri_sentences[i])): #there might be an issue with this thing
            #unigram prob
            unigram_p = unigram_prob.get(uni_sentences[i][k])
            #bigram prob
            if bi_sentences[i][k][0]==START:
                if bigram_prob.get(bi_sentences[i][k]) == None:
                    bigram_p = 0
                else:
                    bigram_p =  bigram_prob.get(bi_sentences[i][k]) / dic.get(STOP) #bigram_prob.get(sentences[i][k])/dic.get(sentences[i][k][0])
            else:
                if bigram_prob.get(bi_sentences[i][k]) == None:
                    bigram_p = 0
                else:
                     bigram_p = (bigram_prob.get(bi_sentences[i][k])/dic.get(bi_sentences[i][k][0])) # add the probablilty calculations here
            #trigram prob
            if trigram_prob.get(tri_sentences[i][k]) == None:
                trigram_p = 0
            else:
                if k == 0:
                    trigram_p = bigram_prob.get((tri_sentences[i][0])) / 61530
                else:
                    trigram_p = trigram_prob.get(tri_sentences[i][k]) / bigram_prob.get((tri_sentences[i][k][0], tri_sentences[i][k][1]))

            prob_sntc += math.log(((unigram_p * lam1) + (bigram_p * lam2) + (trigram_p * lam3)), 2)  # this is currently trying to do log of 0 in some cases which is uhoh

        prob_data = prob_data + prob_sntc

    prob_data = 2 ** ((-1) * prob_data / m)
    return prob_data

#main code portion here
#
#
#
#
#perplexity for training data (unigram)
print("unigram tests")
train = parser("1b_benchmark.train.tokens") #parsed
dic = training(train) #dictionary set
unknown(dic) # unks resolved
sentences = token_sentences(dic, train) # also replaces stuff with unk
probabilities =(unigram_tokens(dic))
print (unigram_perplexity(dic, probabilities, sentences))
#
#
dev = parser("1b_benchmark.dev.tokens")
dev_sentences = token_sentences(dic, dev)
print (unigram_perplexity(dic, probabilities, dev_sentences))
#
#
test = parser("1b_benchmark.test.tokens")
test_sentences = token_sentences(dic, test)
print (unigram_perplexity(dic, probabilities, test_sentences))
#
#
# this is a test of the training set
print("bigram tests")
bigram_training= (bigram_sentence(dic, train))  # train is a list of strings which gets converted to a list of bigrams includes start
bigram_counts = bigram_probability(dic, bigram_training)  # this returns a dic with counts of bigrams this is training data so dont try agian
perplex_bigram_train = bigram_perplexity (dic, bigram_counts, bigram_training)  # this gets perplexity and should return infinite if it tries to explode
print (perplex_bigram_train) # print
#
bigram_dev = (bigram_sentence(dic, dev))
perplex_bigram_dev = bigram_perplexity (dic, bigram_counts, bigram_dev)
print (perplex_bigram_dev)

bigram_test = (bigram_sentence(dic, test))
perplex_bigram_test = bigram_perplexity (dic, bigram_counts, bigram_test)
print (perplex_bigram_test)

#trigram testing
print("trigram tests")
trigram_training = (trigram_sentence(dic,train))
trigram_counts = trigram_probability(dic, trigram_training)
perplex_trigram_train = trigram_perplexity(bigram_counts, trigram_counts, trigram_training)
print (perplex_trigram_train)

trigram_dev = (trigram_sentence(dic, dev))
perplex_trigram_dev = trigram_perplexity (bigram_counts, trigram_counts, trigram_dev)
print (perplex_trigram_dev)

trigram_test = (trigram_sentence(dic, test))
perplex_trigram_test = trigram_perplexity (bigram_counts, trigram_counts, trigram_test)
print (perplex_trigram_test)
#

#smoothing testing
#smoothing should take a straight parse from file and do its own breakdown
#lam1-3 should add to 1
print("smoothing tests")
smooth = (smoothing(.3,.3,.4, dic, probabilities, bigram_counts, trigram_counts, train))
print(smooth)
smooth = (smoothing(.3,.3,.4, dic, probabilities, bigram_counts, trigram_counts, dev))
print(smooth)
smooth = (smoothing(.3,.3,.4, dic, probabilities, bigram_counts, trigram_counts, test))
print(smooth)

smooth = (smoothing(.1,.3,.6, dic, probabilities, bigram_counts, trigram_counts, train))
print(smooth)
smooth = (smoothing(.1,.3,.6, dic, probabilities, bigram_counts, trigram_counts, dev))
print(smooth)

smooth = (smoothing(.6,.3,.1, dic, probabilities, bigram_counts, trigram_counts, train))
print(smooth)
smooth = (smoothing(.6,.3,.1, dic, probabilities, bigram_counts, trigram_counts, dev))
print(smooth)

smooth = (smoothing(.1,.6,.3, dic, probabilities, bigram_counts, trigram_counts, train))
print(smooth)
smooth = (smoothing(.1,.6,.3, dic, probabilities, bigram_counts, trigram_counts, dev))
print(smooth)

smooth = (smoothing(.1,.1,.8, dic, probabilities, bigram_counts, trigram_counts, train))
print(smooth)
smooth = (smoothing(.1,.1, .8, dic, probabilities, bigram_counts, trigram_counts, dev))
print(smooth)

#debug thing from piazza
print("debug tests")
debug = parser("1b_benchmark.tokens")
sentences = token_sentences(dic, debug)
print (unigram_perplexity(dic, probabilities, sentences))

bigram_dev = (bigram_sentence(dic, debug))
perplex_bigram_dev = bigram_perplexity (dic, bigram_counts, bigram_dev)
print (perplex_bigram_dev)

trigram_training = (trigram_sentence(dic,debug))
trigram_counts = trigram_probability(dic, trigram_training)
perplex_trigram_train = trigram_perplexity(bigram_counts, trigram_counts, trigram_training)
print (perplex_trigram_train)
#print (sentences)
#print (bigram_dev)
#print(trigram_training)
#print(debug)
smooth = (smoothing(0.1, 0.3, 0.6, dic, probabilities, bigram_counts, trigram_counts, debug))
print(smooth)


#
""""""
#split training data into bigrams and store probabilties I think into another dictionary
#pass this bigram stuff into bigram perplexity
#bigram perplexity has dic probabilities, bigram probabilities, and data we are opperating on
#what to do if pair from data isnt in bigram probabilities maybe set to 1?


#debug printouts
#print (word_count(probabilities))
#print (unigram_probability(dic, sentences))
#print (word_count(dic))
#print (dic.items())
#print (probabilities.items())
#print (len(list (dic)))
#print (dic.get(STOP))
#print (dic.get("\n"))
#print (dic.get("?\n"))
#print (dic.get("?"))
#print (train)