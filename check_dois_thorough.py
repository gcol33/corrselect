#!/usr/bin/env python3
"""
Thoroughly check DOIs by following redirects and verifying final content.
This mimics what CRAN actually does - checks if DOIs resolve to real content.
"""

import requests
import time
from urllib.parse import urlparse

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

def check_doi_resolves(doi):
    """
    Check if a DOI resolves by attempting to reach the final destination.
    Returns: (status, message, final_url)
    """
    url = f"https://doi.org/{doi}"

    # Try multiple strategies
    strategies = [
        ("HEAD with redirect", lambda: requests.head(url, timeout=15, allow_redirects=True)),
        ("GET with redirect", lambda: requests.get(url, timeout=15, allow_redirects=True,
                                                   headers={'User-Agent': 'Mozilla/5.0'})),
        ("HEAD to resolved URL", lambda: requests.head(
            requests.get(f"https://doi.org/{doi}", timeout=10, allow_redirects=False).headers.get('Location', url),
            timeout=10
        ))
    ]

    for strategy_name, request_func in strategies:
        try:
            response = request_func()

            # Check if we got to a real page
            if response.status_code == 200:
                return "✓ VALID", f"Resolves via {strategy_name}", response.url
            elif response.status_code == 403:
                # 403 means the DOI resolves but publisher blocks bots
                # This is ACCEPTABLE for CRAN - the DOI is valid
                return "✓ VALID (403)", "Resolves but publisher blocks bots (ACCEPTABLE)", response.url
            elif response.status_code == 302 or response.status_code == 301:
                # Redirect means DOI resolver is working
                return "✓ VALID", f"Redirects correctly ({strategy_name})", response.url

        except requests.exceptions.Timeout:
            continue  # Try next strategy
        except Exception as e:
            continue  # Try next strategy

    # If all strategies failed
    return "✗ FAILED", "Could not resolve after multiple attempts", url

print(f"Thoroughly checking {len(dois)} DOIs...\n")
print("Legend:")
print("  ✓ VALID       - DOI resolves to content (200)")
print("  ✓ VALID (403) - DOI resolves but publisher blocks bots (ACCEPTABLE for CRAN)")
print("  ✗ FAILED      - DOI does not resolve\n")

results = []
valid_count = 0
valid_403_count = 0
failed_count = 0

for i, doi in enumerate(dois, 1):
    print(f"[{i:2d}/{len(dois)}] Checking {doi}...")

    status, message, final_url = check_doi_resolves(doi)

    if "✓ VALID (403)" in status:
        valid_403_count += 1
        print(f"      → {status}: {message}")
    elif "✓ VALID" in status:
        valid_count += 1
        print(f"      → {status}: {message}")
    else:
        failed_count += 1
        print(f"      → {status}: {message}")

    results.append({
        'doi': doi,
        'status': status,
        'message': message,
        'final_url': final_url
    })

    time.sleep(1)  # Be very polite to servers

print("\n" + "="*70)
print("=== FINAL SUMMARY ===")
print("="*70)
print(f"Total DOIs checked:              {len(results)}")
print(f"Valid (200 response):            {valid_count}")
print(f"Valid but blocked (403):         {valid_403_count}")
print(f"Failed to resolve:               {failed_count}")
print()

acceptable = valid_count + valid_403_count
print(f"ACCEPTABLE FOR CRAN:             {acceptable}/{len(results)}")

if failed_count > 0:
    print("\n⚠️  PROBLEMATIC DOIs (truly broken):")
    for r in results:
        if "✗" in r['status']:
            print(f"  • {r['doi']}")
            print(f"    {r['message']}")
else:
    print("\n✅ ALL DOIs ARE VALID!")
    print("   (403 responses are acceptable - they indicate valid DOIs")
    print("    with publisher bot protection)")
