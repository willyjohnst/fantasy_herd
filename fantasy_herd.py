from playwright.sync_api import sync_playwright
import pandas as pd
import json
import re
import time

def main():
    api_cows_dict = {}  # Will hold our API data, keyed by cow name
    html_scraped_data = [] # Will hold our UI scraped data

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()

        def handle_response(response):
            if "herd?edit=true" in response.url and response.request.method == "POST":
                try:
                    text = response.text()
                    if "performanceSummaries" in text:
                        print("Intercepted the target API payload!")
                        for line in text.splitlines():
                            if "performanceSummaries" in line:
                                clean_json_str = re.sub(r'^\d+:', '', line)
                                data = json.loads(clean_json_str)
                                items = data.get("data", {}).get("items", [])
                                for item in items:
                                    name = item.get("name")
                                    if name:
                                        api_cows_dict[name] = item
                except Exception as e:
                    pass
        
        page.on("response", handle_response)

        print("Logging in...")
        page.goto("https://www.fantasyherd.co.nz/sign-in")
        page.fill("input[autocomplete='email']", "willyj@duck.com") 
        page.fill("input[autocomplete='current-password']", "7DPv,ZPta)C?g%%")
        page.click("button[type='submit']")

        page.wait_for_url("https://www.fantasyherd.co.nz/")
        page.goto("https://www.fantasyherd.co.nz/herd?edit=true")
        page.wait_for_url("https://www.fantasyherd.co.nz/herd*")
        page.wait_for_timeout(10000) 
        page.wait_for_load_state("networkidle") 

        print("Hunting for the correct API payload...")
        timeout = 120 
        start_time = time.time()
        while not api_cows_dict and (time.time() - start_time) < timeout:
            page.wait_for_timeout(20000) 
            if not api_cows_dict:
                print("Reloading...")
                page.reload()
            
        print(f"Successfully loaded {len(api_cows_dict)} cows from the API.")

        cow_buttons = page.locator("//button[@class='flex gap-3 items-center w-full']").all()
        print(f"Found {len(cow_buttons)} UI buttons to click.")

        # ------------------------------------------------------------------
        # Helper: read the "Total Points" value currently displayed in the
        # drawer's data table. Returns an int, or None if it can't be read.
        # ------------------------------------------------------------------
        def read_displayed_total_points():
            """Read the Total Points value from the currently visible table."""
            try:
                table_rows = page.locator(
                    "//table[contains(@class, 'w-full') and contains(@class, 'table-fixed')]//tbody//tr"
                ).all()
                for row in table_rows:
                    cols = row.locator("td").all()
                    if len(cols) == 2:
                        label = cols[0].inner_text(timeout=2000).strip()
                        if label == "Total Points":
                            return int(cols[1].inner_text(timeout=2000).strip())
            except:
                pass
            return None

        # ------------------------------------------------------------------
        # Helper: close the drawer reliably
        # ------------------------------------------------------------------
        def close_drawer():
            """Close the cow-details drawer, trying multiple strategies."""
            for strategy in range(3):
                try:
                    if strategy == 0:
                        page.keyboard.press("Escape")
                    elif strategy == 1:
                        page.locator('div[data-slot="sheet-overlay"]').click(
                            force=True, position={"x": 10, "y": 10}
                        )
                    else:
                        # Last resort: click the X button
                        close_btn = page.locator(
                            'button:has(svg.lucide-x)'
                        ).first
                        if close_btn.is_visible():
                            close_btn.click(timeout=3000)

                    page.locator('div[data-slot="sheet-overlay"]').wait_for(
                        state="hidden", timeout=3000
                    )
                    return  # success
                except:
                    continue

            # If nothing worked, wait and hope the next click overrides it
            page.wait_for_timeout(1000)

        # ------------------------------------------------------------------
        # Main scraping loop
        # ------------------------------------------------------------------
        for index, button in enumerate(cow_buttons):
            print(f"Scraping cow {index + 1} of {len(cow_buttons)}...")
            
            cow_stats = {}
            success = False
            max_retries = 5
            
            for attempt in range(max_retries):
                cow_stats = {}  # reset on each attempt
                try:
                    button.scroll_into_view_if_needed()
                    button.click(timeout=10000)

                    # --- WAIT FOR DRAWER WITH FRESH DATA ---
                    # Step 1: Wait for the table element to appear
                    page.locator(
                        "//table[contains(@class, 'w-full') and contains(@class, 'table-fixed')]"
                    ).wait_for(state="visible", timeout=15000)
                    
                    # Step 2: Wait for network activity to settle (data fetch)
                    page.wait_for_load_state("networkidle")

                    # Step 3: Small buffer for React state to propagate to DOM
                    page.wait_for_timeout(500)

                    # Step 4: Scrape the cow's name from the h4
                    cow_name = page.locator(
                        '//h4[contains(@class, "font-title text-[48px] leading")]'
                    ).inner_text(timeout=5000).strip()
                    cow_stats["name"] = cow_name
                    name_key = cow_name.split('\n')[0].strip()

                    # Step 5: VALIDATE that the table data belongs to this cow.
                    # Compare the displayed Total Points against the API's
                    # current-week points for this cow. If they don't match,
                    # the drawer is showing stale data from a previous cow.
                    api_data = api_cows_dict.get(name_key, {})
                    summaries = api_data.get("performanceSummaries", [])
                    expected_pts = summaries[0].get("totalPoints") if summaries else None

                    displayed_pts = read_displayed_total_points()

                    if expected_pts is not None and displayed_pts is not None:
                        if displayed_pts != expected_pts:
                            print(f"  -> DATA STALE for {name_key}: "
                                  f"displayed={displayed_pts}, api={expected_pts}. "
                                  f"Waiting for table to update...")
                            
                            # Wait up to 5 seconds for the table to refresh,
                            # polling every 500ms
                            stale = True
                            for _ in range(10):
                                page.wait_for_timeout(500)
                                displayed_pts = read_displayed_total_points()
                                if displayed_pts == expected_pts:
                                    stale = False
                                    print(f"  -> Table updated! Now showing {displayed_pts}.")
                                    break
                            
                            if stale:
                                print(f"  -> Still stale after 5s. Closing and retrying...")
                                close_drawer()
                                page.wait_for_timeout(1000)
                                continue  # retry this cow

                    # --- SCRAPE ROUND-BY-ROUND DATA ---
                    # Isolate the week pagination container (use .last to avoid
                    # the cow-navigation container which has identical structure)
                    pagination_container = page.locator(
                        "//div[contains(@class, 'grow flex items-center justify-center gap-2')]"
                    ).last
                    
                    has_history = False
                    try:
                        pagination_container.wait_for(state="visible", timeout=3000)
                        has_history = True
                    except:
                        pass 

                    if has_history:
                        while True:
                            # Read the current week label
                            week_spans = pagination_container.locator("span").all()
                            if len(week_spans) > 0:
                                current_week = week_spans[-1].inner_text(timeout=3000).strip()
                            else:
                                break

                            # Scrape the data table
                            table_rows = page.locator(
                                "//table[contains(@class, 'w-full') and contains(@class, 'table-fixed')]//tbody//tr"
                            ).all()
                            
                            for row in table_rows:
                                cols = row.locator("td").all()
                                if len(cols) == 2 and cols[0].inner_text().strip() != "Captain Bonus":
                                    label = cols[0].inner_text().strip()
                                    value = cols[1].inner_text().strip()
                                    cow_stats[f"html_{current_week}_{label}"] = value

                            if "DRW" in current_week:
                                break
                            
                            # Navigate to previous week using the LAST border-r
                            # button (the week nav, not the cow nav)
                            left_button = page.locator("//div[@class='border-r']//button").last
                            if left_button:
                                prev_btn = left_button
                            else:
                                break
                            
                            if prev_btn.is_disabled() or prev_btn.get_attribute("disabled") is not None:
                                break
                            
                            prev_btn.click(timeout=5000)
                            page.wait_for_timeout(800)
                    
                    # --- CLOSE THE DRAWER ---
                    close_drawer()
                    
                    success = True
                    break 

                except Exception as e:
                    print(f"  -> Attempt {attempt + 1} failed for cow {index + 1}. Error: {e}")
                    close_drawer()
                    page.wait_for_timeout(2000)

            if success:
                html_scraped_data.append(cow_stats)
            else:
                print(f"  -> Skipping cow {index + 1} completely after {max_retries} failed attempts.")
        
        # ------------------------------------------------------------------
        # Join HTML data with API data
        # ------------------------------------------------------------------
        print("Joining UI data with API data...")
        final_dataset = []
        seen_names = set()
        
        for html_row in html_scraped_data:
            name_key = html_row["name"].split('\n')[0].strip()
            api_data = api_cows_dict.get(name_key, {})
            
            # Deduplicate: if we've already seen this cow (from the team
            # section), and this is the second occurrence (from the browse
            # section), keep the one whose HTML data matches the API.
            summaries = api_data.get("performanceSummaries", [])
            api_pts = summaries[0].get("totalPoints") if summaries else None
            
            # Find the latest round's Total Points in the HTML data
            html_latest_pts = None
            for key in ["html_ROUND 4_Total Points", "html_ROUND 3_Total Points",
                        "html_ROUND 2_Total Points", "html_ROUND 1_Total Points",
                        "html_DRW_Total Points"]:
                if key in html_row:
                    html_latest_pts = int(html_row[key])
                    break
            
            ear_tag = html_row["name"].split('\n')[1].strip() if '\n' in html_row["name"] else ""
            unique_key = f"{name_key}_{ear_tag}"
            
            if unique_key in seen_names:
                # This is a duplicate. Replace only if this copy is correct.
                if api_pts is not None and html_latest_pts == api_pts:
                    # This copy matches API — replace the old one
                    final_dataset = [
                        r for r in final_dataset
                        if not (r["name"].split('\n')[0].strip() == name_key and
                                r.get("_ear_tag") == ear_tag)
                    ]
                    print(f"  Replacing stale duplicate for {name_key} with verified copy.")
                else:
                    # This copy also doesn't match, skip it
                    print(f"  Skipping second copy of {name_key} (also unverified).")
                    continue
            
            seen_names.add(unique_key)
            
            merged_row = html_row.copy()
            merged_row["_ear_tag"] = ear_tag  # temp field for dedup
            
            merged_row["api_id"] = api_data.get("id")
            merged_row["api_earTag"] = api_data.get("earTag")
            merged_row["api_price"] = api_data.get("price")
            merged_row["api_rating"] = api_data.get("rating")
            merged_row["api_breedingWorth"] = api_data.get("breedingWorth")
            merged_row["api_productionWorth"] = api_data.get("productionWorth")
            merged_row['api_historicMilkVolume'] = api_data.get("historicMilkVolume")
            merged_row['api_historicProtein'] = api_data.get("historicProtein")
            merged_row['api_isResting'] = api_data.get("isResting")
            
            if summaries:
                merged_row["api_current_week_points"] = summaries[0].get("totalPoints")
                merged_row["api_gameWeekId"] = summaries[0].get("gameWeekId")
            
            # Flag whether this row's data was verified against the API
            merged_row["data_verified"] = (
                api_pts is not None and html_latest_pts is not None and
                html_latest_pts == api_pts
            )
            
            final_dataset.append(merged_row)
        
        # Clean up temp field
        for row in final_dataset:
            row.pop("_ear_tag", None)

        df = pd.DataFrame(final_dataset)
        
        verified = df["data_verified"].sum() if "data_verified" in df.columns else 0
        total = len(df)
        print(f"\nData quality: {verified}/{total} rows verified against API ({100*verified/total:.1f}%)")
        
        df.to_csv("fantasy_herd_joined_data.csv", index=False)
        print(f"Success! Saved {len(df)} joined records to fantasy_herd_joined_data.csv")

        browser.close()

if __name__ == "__main__":
    main()