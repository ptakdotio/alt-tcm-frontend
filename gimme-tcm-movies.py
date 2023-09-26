import json
import os
import requests
import time

from argparse import ArgumentParser


TCM_MOVIES_JSON = 'tcm-movies.json'

TCM_GENRES = {
    'Romance', 'Silent', 'Short', 'Western', 'Spy', 'Horror', 'Prison', 'Erotic', 'Political', 'Action', 'Sports',
    'Nature', 'Musical', 'Dance', 'Thriller', 'Biography', 'Interview', 'Crime', 'Family', 'Documentary', 'Children',
    'Teens', 'Animation', 'Drama', 'Martial Arts', 'Romantic Comedy', 'Comedy', 'Educational', 'Film Noir', 'Satire',
    'Adaptation', 'Foreign', 'Period', 'Horror/Science-Fiction', 'Adventure', 'Suspense/Mystery', 'Historical', 'War',
    'Fantasy', 'Disaster', 'Classic Hollywood', 'Music',
}

def tcm_genres(value):
    genres = []
    for genre in value.split(','):
        genre = genre.strip()
        if genre not in TCM_GENRES:
            raise ValueError(f'{genre} is not a valid TCM genre')
        genres.append(genre)
    return set(genres)

def file_age(filename):
    return time.time() - os.path.getmtime(filename)


def get_tcm_movies_list():
    if os.path.exists(TCM_MOVIES_JSON) and file_age(TCM_MOVIES_JSON) < (24 * 60 * 60):
        with open(TCM_MOVIES_JSON, 'r') as f:
            movies = json.load(f)
    else:
        print('Downloading movies list...')
        res = requests.get('https://tcmws.tcm.com/tcmws/latest/250')
        movies = res.json()['tcm']['titles']
        with open(TCM_MOVIES_JSON, 'w') as f:
            json.dump(movies, f)
    return movies


def main():
    parser = ArgumentParser()
    parser.add_argument('--before-year', type=int)
    parser.add_argument('--after-year', type=int)
    parser.add_argument('--genres', type=tcm_genres)
    parser.add_argument('--list-genres', action='store_true')
    parser.add_argument('--html', action='store_true')
    args = parser.parse_args()

    if args.list_genres:
        for genre in sorted(TCM_GENRES):
            print(genre)
        return

    if args.html:
        print('<table>')
        print('  <tr>')
        print('    <th>Year</th>')
        print('    <th>Title</th>')
        print('    <th>Description</th>')
        print('  </tr>')

    # Create list of movies
    for movie in get_tcm_movies_list():

        release_year = movie['releaseYear']
        title        = movie['name']
        genres       = movie['tvGenresArr'] or []
        description  = movie['description'] or ''

        if args.before_year and release_year >= args.before_year:
            continue
        if args.after_year and release_year <= args.after_year:
            continue

        if args.genres and not args.genres.intersection(genres):
            continue

        if args.html:
            print(f'  <tr>')
            print(f'    <td>{release_year}</td>')
            print(f'    <td>{title}</td>')
            print(f'    <td>{description}</td>')
            print(f'  </tr>')
        else:
            print(f'{str(release_year):10} {title[:40]:40} {description}')
            
    if args.html:
        print('</table>')


if __name__ == '__main__':
    main()

