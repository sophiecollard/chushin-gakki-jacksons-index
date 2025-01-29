import boto3
import json
import jsonpath_ng
import os
from typing import List

spaces_key = os.getenv('SPACES_KEY')
spaces_secret = os.getenv('SPACES_SECRET')

s3_client = boto3.client(
    's3',
    endpoint_url='https://ams3.digitaloceanspaces.com',
    aws_access_key_id=spaces_key,
    aws_secret_access_key=spaces_secret,
)

def get_keys(s3_client, bucket: str, prefix: str) -> List[str]:
    """Returns the list of keys in the specified bucket and with the specified prefix"""
    keys = []
    for obj in s3_client.list_objects(Bucket=bucket, Prefix=prefix)['Contents']:
        key = obj['Key']
        if key != prefix:
            keys.append(key)
        else:
            continue
    return keys

def read_obj(s3_client, bucket: str, key: str) -> str:
    """Reads the contents of the object at the specified key"""
    data = s3_client.get_object(Bucket=bucket, Key=key)
    contents = data['Body'].read().decode("utf-8")
    return contents

def extract_str_at(path: str, payload: dict) -> List[str]:
    """Extacts the string at the specified JSON path"""
    jsonpath_expression = jsonpath_ng.parse(path)
    matches = jsonpath_expression.find(payload)
    values = []
    for m in matches:
        values.append(str(m.value))
    return values

def flatten(xss: List[str]):
    """Flattens nested lists"""
    return [x for xs in xss for x in xs]

def break_into_tokens(token_size: int, string: str) -> List[str]:
    """Breaks down a string into tokens of the specified size"""
    tokens = []
    for i in range(0, len(string) - (token_size - 1)):
        tokens.append(string[i:i+token_size])
    return tokens

def split_on_shape_acronym(string: str) -> List[str]:
    """Splits a string into separate tokens if starting w/ shape acronym"""
    shape_acronyms = [ 'asl', 'dk', 'ke', 'ks', 'kv', 'rr', 'sl', 'wr' ]
    if len(string) > 2 and string[:2] in shape_acronyms:
        return [ string[:2], string[2:] ]
    else:
        return [ string ]

def add_alternative(string: str) -> List[str]:
    alt = {
        'dk': 'dinky',
        'ke': 'kelly',
        'kv': 'king',
        'rr': 'rhoads',
        'sl': 'soloist',
        'wr': 'warrior',
        'dinky': 'dk',
        'kelly': 'ke',
        'king': 'kv',
        'rhoads': 'rr',
        'soloist': 'sl',
        'warrior': 'wr'
    }
    if string in alt.keys():
        return [ string, alt.get(string) ]
    else:
        return [ string ]

def tokenize(string: str) -> List[str]:
    """Breaks a string down into tokens"""
    split_on_whitespace = lambda s : s.split(' ')
    split_on_dash = lambda s : s.split('-')
    split_on_slash = lambda s : s.split('/')
    basic_tokens = [ string.lower() ]
    basic_tokens = flatten(map(split_on_whitespace, basic_tokens))
    basic_tokens = flatten(map(split_on_dash, basic_tokens))
    basic_tokens = flatten(map(split_on_slash, basic_tokens))
    basic_tokens = list(filter(lambda t : t != 'jackson', basic_tokens))
    basic_tokens = flatten(map(split_on_shape_acronym, basic_tokens))
    basic_tokens = flatten(map(add_alternative, basic_tokens))

    tokens_longer_than_2 = list(filter(lambda s : len(s) > 2, basic_tokens))
    tokens_longer_than_3 = list(filter(lambda s : len(s) > 3, basic_tokens))

    size2_tokens = flatten(map(lambda s : break_into_tokens(2, s), tokens_longer_than_2))
    size3_tokens = flatten(map(lambda s : break_into_tokens(3, s), tokens_longer_than_3))

    all_tokens = basic_tokens + size2_tokens + size3_tokens
    all_tokens = list(filter(lambda s : len(s) > 0, all_tokens))
    return all_tokens

def tokenize_entry(entry: dict) -> List[str]:
    """Tokenises a DB entry"""
    brand_tokens = tokenize(entry.get('brand'))
    model_tokens = tokenize(entry.get('model'))
    all_tokens = brand_tokens + model_tokens
    return all_tokens

def jaccard_similarity(query_tokens: List[str], entry_tokens: List[str]) -> float:
    """Compute Jaccard similarity between two sets of tokens"""
    token_count_intersection = len(set(query_tokens) & set(entry_tokens))
    token_count_union = len(set(query_tokens + entry_tokens))
    
    if token_count_union > 0:
        return token_count_intersection / token_count_union
    
    else:
        return 0.0

s3_client = boto3.client(
    's3',
    endpoint_url='https://ams3.digitaloceanspaces.com',
    aws_access_key_id=spaces_key,
    aws_secret_access_key=spaces_secret,
)

keys = get_keys(s3_client, bucket='jackson', prefix='db/')

index = []
for key in keys:
    print('key: {}'.format(key))
    raw_json = read_obj(s3_client, bucket='jackson', key=key)
    db_entry = json.loads(raw_json)
    tokens = tokenize_entry(db_entry)
    index_entry = {
        'name': '{} {}'.format(db_entry.get('brand'), db_entry.get('model')),
        'url': 'https://jackson.ams3.digitaloceanspaces.com/{}'.format(key),
        'tokens': tokens
    }
    index.append(index_entry)

# query = 'jackson stars rrj1'
# query = 'rrtn01/emg'
query = 'stars rrj2 special'
# query = 'jackson rr24'
query_tokens = tokenize(query)
print('Query: {}'.format(query))

scored_entries = []
for entry in index:
    name = entry.get('name')
    url = entry.get('url')
    score = jaccard_similarity(query_tokens, entry.get('tokens'))
    scored_entries.append({ 'score': score, 'name': name, 'url': url })

sorted_scored_entries = sorted(scored_entries, key=lambda x: x.get('score'), reverse=True)
for entry in sorted_scored_entries[:5]:
    print(entry['name'])
    print(entry['url'])
    print(entry['score'])
    print('')
