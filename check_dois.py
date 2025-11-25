#!/usr/bin/env python3
"""
Check all DOIs in the corrselect package.
Uses browser-like headers to avoid 403 errors.
"""

import requests
import time
from urllib.parse import quote

# List of all DOIs in the package
dois = [
    "10.18637/jss.v028.i05",
    "10.18637/jss.v036.i11",
    "10.18637/jss.v033.i01",
    "10.1002/0471725153",
    "10.1007/978-3-642-17517-6_36",
    "10.1145/362342.362367",
    "10.1016/j.tcs.2006.06.015",
    "10.1145/2402.322385",
    "10.1137/0206036",
    "10.1007/s11135-006-9018-6",
    "10.2307/1267205",
    "10.2307/1412159",
    "10.2307/2332226",
    "10.1016/S0167-5060(13)71063-X",
    "10.1007/BF02760024",
    "10.1111/j.1600-0587.2012.07348.x",
    "10.1093/bioinformatics/btm344"
]

# Browser-like headers to avoid 403
headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
    'Accept-Language': 'en-US,en;q=0.5',
    'Accept-Encoding': 'gzip, deflate',
    'DNT': '1',
    'Connection': 'keep-alive',
    'Upgrade-Insecure-Requests': '1'
}

print(f"Checking {len(dois)} DOIs with browser-like headers...\n")

results = []
ok_count = 0
redirect_count = 0
failed_count = 0
error_count = 0

for doi in dois:
    url = f"https://doi.org/{doi}"

    try:
        # Use HEAD request first (faster)
        response = requests.head(url, headers=headers, timeout=10, allow_redirects=True)
        status_code = response.status_code

        # If HEAD fails, try GET
        if status_code >= 400:
            response = requests.get(url, headers=headers, timeout=10, allow_redirects=True)
            status_code = response.status_code

        if status_code == 200:
            status = "OK"
            ok_count += 1
            symbol = "✓"
        elif 300 <= status_code < 400:
            status = "REDIRECT"
            redirect_count += 1
            symbol = "↻"
        else:
            status = "FAILED"
            failed_count += 1
            symbol = "✗"

        print(f"{symbol} {doi:45s} [{status_code}]")

        results.append({
            'doi': doi,
            'status': status,
            'code': status_code,
            'final_url': response.url if hasattr(response, 'url') else url
        })

    except requests.exceptions.Timeout:
        print(f"✗ {doi:45s} [TIMEOUT]")
        results.append({'doi': doi, 'status': 'TIMEOUT', 'code': None, 'final_url': None})
        error_count += 1

    except Exception as e:
        print(f"✗ {doi:45s} [ERROR: {str(e)[:30]}]")
        results.append({'doi': doi, 'status': 'ERROR', 'code': None, 'final_url': None})
        error_count += 1

    time.sleep(0.5)  # Be polite

print("\n=== SUMMARY ===")
print(f"Total DOIs checked: {len(results)}")
print(f"OK (200):           {ok_count}")
print(f"Redirect (3xx):     {redirect_count}")
print(f"Failed (4xx/5xx):   {failed_count}")
print(f"Errors/Timeouts:    {error_count}")

if failed_count > 0 or error_count > 0:
    print("\n=== PROBLEMATIC DOIs ===")
    for r in results:
        if r['status'] not in ['OK', 'REDIRECT']:
            print(f"  {r['doi']:45s} [{r['status']}]")
else:
    print("\n✅ All DOIs are accessible!")
